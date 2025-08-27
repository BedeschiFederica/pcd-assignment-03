package pcd.ass03.model

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, StashBuffer}
import akka.cluster.ddata.{LWWMap, LWWMapKey}
import akka.cluster.ddata.typed.scaladsl.{DistributedData, Replicator}
import Replicator.{Get, GetResponse, ReadLocal, Update, UpdateResponse, WriteLocal}
import pcd.ass03.Message
import pcd.ass03.model.EatingManager.EatingManagerMessage
import pcd.ass03.view.{GlobalView, PlayerView}
import GlobalView.GlobalViewMessage
import PlayerActor.PlayerMessage
import PlayerMessage.*
import PlayerView.PlayerViewMessage
import PlayerViewMessage.RenderWorld
import pcd.ass03.model.EndGameManager.EndGameManagerMessage

import scala.util.Random
import concurrent.duration.FiniteDuration

object WorldManager:

  trait WorldMessage extends Message
  object WorldMessage:
    case class SendPlayer(player: Player, from: ActorRef[PlayerMessage]) extends WorldMessage
    case class SendWorld(world: World) extends WorldMessage
    case class Tick() extends WorldMessage
    case class UpdateWorld() extends WorldMessage
    case class UpdatedWorld(world: World) extends WorldMessage
    case class EndGame(winner: Player) extends WorldMessage
    case class InternalUpdateResponse(rsp: UpdateResponse[LWWMap[String, World]]) extends WorldMessage
    case class InternalGetResponse(rsp: GetResponse[LWWMap[String, World]]) extends WorldMessage

  import WorldMessage.*

  private val InitialFoodMass = 100.0
  private val FoodsNumber = 30

  val WorldKey = "worldState"
  val DataKey: LWWMapKey[String, World] = LWWMapKey(WorldKey)
  val Service: ServiceKey[WorldMessage] = ServiceKey[WorldMessage]("WorldService")
  def apply(width: Int, height: Int)(frameRate: FiniteDuration) : Behavior[WorldMessage | Receptionist.Listing] =
    Behaviors.setup: context =>
      context.spawnAnonymous(EatingManager())
      context.spawnAnonymous(EndGameManager())
      context.spawnAnonymous(GlobalView(width, height)(frameRate))

      val replicator = DistributedData(context.system).replicator
      replicator ! Get(DataKey, ReadLocal, context.messageAdapter(InternalGetResponse.apply))

      context.system.receptionist ! Receptionist.Register(Service, context.self)
      Set(PlayerView.Service, GlobalView.Service, PlayerActor.Service, EatingManager.Service, EndGameManager.Service)
        .foreach(context.system.receptionist ! Receptionist.Subscribe(_, context.messageAdapter(listing => listing)))

      Behaviors.withTimers: timers =>
        timers.startTimerAtFixedRate(Tick(), frameRate)
        WorldImpl(World(width, height, List.empty, initialFoods(FoodsNumber, width, height)), context, replicator)
          .receive

  private def initialFoods(numFoods: Int, width: Int, height: Int, initialMass: Double = InitialFoodMass): Seq[Food] =
    (1 to numFoods).map(i => Food(s"f$i", Position(Random.nextInt(width), Random.nextInt(height)), initialMass))

  private case class WorldImpl(private var world: World, ctx: ActorContext[WorldMessage | Receptionist.Listing],
                               replicator: ActorRef[Replicator.Command]):
    private val StashBufferSize = 1000
    private var players: Seq[ActorRef[PlayerMessage]] = List.empty
    private var playerViews: Seq[ActorRef[PlayerViewMessage]] = List.empty
    private var eatingManager: Option[ActorRef[EatingManagerMessage]] = Option.empty
    private var endGameManager: Option[ActorRef[EndGameManagerMessage]] = Option.empty
    private var globalView: Option[ActorRef[GlobalViewMessage]] = Option.empty
    private val updateAdapter = ctx.messageAdapter(InternalUpdateResponse.apply)
    private var counter = 0

    val receive: Behavior[WorldMessage | Receptionist.Listing] = Behaviors.receiveMessagePartial:
      case InternalGetResponse(rsp) => rsp match
        case Replicator.GetSuccess(`DataKey`) =>
          val data: LWWMap[String, World] = rsp.asInstanceOf[Replicator.GetSuccess[LWWMap[String, World]]].get(DataKey)
          data.get(WorldKey).foreach(world = _)
        case _ => ()
        Behaviors.same
      case msg: Receptionist.Listing =>
        msg.key match
          case PlayerView.Service => playerViews = msg.serviceInstances(PlayerView.Service).toList
          case GlobalView.Service => globalView = msg.getManager(GlobalView.Service, globalView)
          case PlayerActor.Service => players = msg.serviceInstances(PlayerActor.Service).toList
          case EatingManager.Service => eatingManager = msg.getManager(EatingManager.Service, eatingManager)
          case EndGameManager.Service => endGameManager = msg.getManager(EndGameManager.Service, endGameManager)
        Behaviors.same
      case Tick() =>
        if players.nonEmpty then
          world = world.copy(players = List.empty)
          players.foreach(_ ! PlayerMessage.Ask(world, ctx.self))
          waitingValues
        else
          Behaviors.same
      case UpdateWorld() =>
        eatingManager.foreach(_ ! EatingManagerMessage.UpdateWorld(world, ctx.self))
        Behaviors.same
      case UpdatedWorld(newWorld) =>
        newWorld.updateAfterEating()
        world = newWorld
        replicator ! Update(DataKey, LWWMap.empty[String, World], WriteLocal, updateAdapter): map =>
          (map :+ (WorldKey -> world))(using DistributedData(ctx.system).selfUniqueAddress)
        updateViews()
        endGameManager.foreach(_ ! EndGameManagerMessage.CheckEndGame(world, ctx.self))
        Behaviors.same
      case EndGame(winner) =>
        playerViews.foreach(_ ! PlayerViewMessage.EndGame(winner))
        ctx.system.terminate()
        Behaviors.stopped

    private def updateViews(): Unit =
      playerViews.foreach(_ ! PlayerViewMessage.RenderWorld(world))
      globalView.foreach(_ ! GlobalViewMessage.RenderWorld(world))

    extension (msg: Receptionist.Listing)
      private def getManager[T](service: ServiceKey[T], manager: Option[ActorRef[T]]): Option[ActorRef[T]] =
        Option(msg.serviceInstances(service).toList).collect {case l if l.nonEmpty => l.head}

    extension (newWorld: World)
      private def updateAfterEating(): Unit =
        extension (newWorld: World)
          private def findModifiedPlayers: Seq[Player] = newWorld.players.filter(p => p != world.playerById(p.id))
          private def findAlivePlayersActor: Seq[ActorRef[PlayerMessage]] = newWorld.players.map(findActorById)
          private def findEatenPlayers: Seq[ActorRef[PlayerMessage]] = players diff newWorld.findAlivePlayersActor
          private def findEatenPlayersView: Seq[ActorRef[PlayerViewMessage]] =
            world.players.collect {case p if newWorld.playerById(p.id).isEmpty => p.findViewById}

        extension (player: Player)
          private def findActorById: ActorRef[PlayerMessage] = players.find(_.path.name == player.id).get
          private def findViewById: ActorRef[PlayerViewMessage] =
            val playerViewName = PlayerView.getClass.getSimpleName.dropRight(1)
            playerViews.find(_.path.name.drop(playerViewName.length) == player.id.drop(1)).get

        newWorld.findModifiedPlayers.foreach(player => player.findActorById ! Grow(player))
        newWorld.findEatenPlayersView.foreach(_ ! PlayerViewMessage.Stop())
        newWorld.findEatenPlayers.foreach(_ ! PlayerMessage.Stop())
        players = newWorld.findAlivePlayersActor

    private val waitingValues: Behavior[WorldMessage | Receptionist.Listing] =
      Behaviors.withStash[WorldMessage | Receptionist.Listing](StashBufferSize): stash =>
        Behaviors.receiveMessagePartial:
          case SendPlayer(player, from) =>
            counter += 1
            world = world.copy(players = player +: world.players)
            stash.unstashAfterReceivedAll()
          case msg: Receptionist.Listing => msg.key match
            case PlayerActor.Service =>
              val playerServices = msg.serviceInstances(PlayerActor.Service).toList
              if players != playerServices then
                players = playerServices
                stash.unstashAfterReceivedAll()
              else
                Behaviors.same
            case _ => stash.stashIfNotFull(msg)
          case message => stash.stashIfNotFull(message)

    extension (stash: StashBuffer[WorldMessage | Receptionist.Listing])
      private def unstashAfterReceivedAll(): Behavior[WorldMessage | Receptionist.Listing] =
        if counter == players.size then
          counter = 0
          stash.stash(UpdateWorld())
          stash.unstashAll(receive)
        else
          waitingValues

      private def stashIfNotFull(
                                  message: WorldMessage | Receptionist.Listing
                                ): Behavior[WorldMessage | Receptionist.Listing] =
        if stash.size < StashBufferSize - 1 then stash.stash(message)
        Behaviors.same