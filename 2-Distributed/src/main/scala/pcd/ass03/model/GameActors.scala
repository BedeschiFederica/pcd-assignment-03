package pcd.ass03.model

import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import pcd.ass03.Message
import pcd.ass03.model.EatingManager.EndGameManager
import pcd.ass03.model.EatingManager.EndGameManager.EndGameManagerMessage
import pcd.ass03.model.EatingManager.EndGameManager.EndGameManagerMessage.CheckEndGame
import pcd.ass03.model.WorldManager.WorldMessage
import pcd.ass03.view.GlobalView.GlobalViewMessage
import pcd.ass03.view.{GlobalView, PlayerView}
import pcd.ass03.view.PlayerView.PlayerViewMessage

import scala.util.Random
import concurrent.duration.DurationInt

object PlayerActor:
  import WorldMessage.*

  trait PlayerMessage extends Message
  object PlayerMessage:
    case class Move(dx: Double, dy: Double) extends PlayerMessage
    case class Ask(from: ActorRef[WorldMessage]) extends PlayerMessage
    case class Grow(player: Player) extends PlayerMessage
    case class UpdatePos(pos: Position) extends PlayerMessage

  import PlayerMessage.*

  val Service: ServiceKey[PlayerMessage] = ServiceKey[PlayerMessage]("PlayerService")
  def apply(id: String, pos: Position, mass: Double)
           (width: Int, height: Int): Behavior[PlayerMessage | Receptionist.Listing] =
    Behaviors.setup:
      context =>
        context.system.receptionist ! Receptionist.Register(Service, context.self)
        val listingAdapter: ActorRef[Receptionist.Listing] = context.messageAdapter(listing => listing)
        context.system.receptionist ! Receptionist.Subscribe(PlayerView.Service, listingAdapter)
        PlayerImpl(Player(id, pos, mass), context)(width, height).receive

  private case class PlayerImpl(private var player: Player, ctx: ActorContext[PlayerMessage | Receptionist.Listing])
                               (width: Int, height: Int):
    private val Speed = 10.0
    private val playerViewName = PlayerView.getClass.getSimpleName.dropRight(1)
    private var playerView: Option[ActorRef[PlayerViewMessage]] = Option.empty

    val receive: Behavior[PlayerMessage | Receptionist.Listing] = Behaviors.receiveMessagePartial:
      case msg: Receptionist.Listing =>
        val service = msg.serviceInstances(PlayerView.Service).toList
          .find(_.path.name == s"$playerViewName${player.id.drop(1)}")
        if service.nonEmpty && playerView != service then
          playerView = service
          playerView.get ! PlayerViewMessage.Render(player, ctx.self)
        Behaviors.same
      case Move(dx, dy) =>
        player = player.copy(pos = Position((player.pos.x + dx * Speed).max(0).min(width),
          (player.pos.y + dy * Speed).max(0).min(height)))
        if playerView.nonEmpty then playerView.get ! PlayerViewMessage.Render(player, ctx.self)
        Behaviors.same
      case Ask(from) =>
        from ! SendPlayer(player, ctx.self)
        Behaviors.same
      case UpdatePos(newPos) =>
        player = player.copy(pos = newPos)
        Behaviors.same
      case Grow(newPlayer) =>
        player = newPlayer
        Behaviors.same

object EatingManager:
  import WorldManager.WorldMessage.UpdatedWorld

  trait EatingManagerMessage extends Message
  object EatingManagerMessage:
    case class UpdateWorld(world: World, from: ActorRef[WorldMessage]) extends EatingManagerMessage

  import EatingManagerMessage.*

  val Service: ServiceKey[EatingManagerMessage] = ServiceKey[EatingManagerMessage]("FoodManagerService")
  def apply(): Behavior[EatingManagerMessage] =
    Behaviors.setup:
      context =>
        context.system.receptionist ! Receptionist.Register(Service, context.self)
        Behaviors.receiveMessagePartial:
          case UpdateWorld(newWorld, from) =>
            from ! UpdatedWorld(EatingLogic.updateWorld(newWorld))
            Behaviors.same

  object EndGameManager:
    import WorldManager.WorldMessage.EndGame

    trait EndGameManagerMessage extends Message
    object EndGameManagerMessage:
      case class CheckEndGame(world: World, from: ActorRef[WorldMessage]) extends EndGameManagerMessage

    import EndGameManagerMessage.*

    val Service: ServiceKey[EndGameManagerMessage] = ServiceKey[EndGameManagerMessage]("EndGameManagerService")
    def apply(): Behavior[EndGameManagerMessage] =
      Behaviors.setup:
        context =>
          context.system.receptionist ! Receptionist.Register(Service, context.self)
          Behaviors.receiveMessagePartial:
            case CheckEndGame(world, from) =>
              val winner = EndGameLogic.getGameWinner(world.players)
              if winner.nonEmpty then from ! EndGame(winner.get)
              Behaviors.same

object WorldManager:
  import PlayerActor.PlayerMessage
  import PlayerMessage.*
  import pcd.ass03.view.PlayerView.PlayerViewMessage
  import PlayerViewMessage.RenderWorld
  import EatingManager.EatingManagerMessage

  trait WorldMessage extends Message
  object WorldMessage:
    case class SendPlayer(player: Player, from: ActorRef[PlayerMessage]) extends WorldMessage
    case class SendWorld(world: World) extends WorldMessage
    case class Tick() extends WorldMessage
    case class UpdateWorld() extends WorldMessage
    case class UpdatedWorld(world: World) extends WorldMessage
    case class EndGame(winner: Player) extends WorldMessage

  import WorldMessage.*

  val Service: ServiceKey[WorldMessage] = ServiceKey[WorldMessage]("WorldService")
  def apply(width: Int, height: Int): Behavior[WorldMessage | Receptionist.Listing] =
    Behaviors.setup:
      context =>
        context.system.receptionist ! Receptionist.Register(Service, context.self)
        val listingAdapter: ActorRef[Receptionist.Listing] = context.messageAdapter(listing => listing)
        context.system.receptionist ! Receptionist.Subscribe(PlayerView.Service, listingAdapter)
        context.system.receptionist ! Receptionist.Subscribe(GlobalView.Service, listingAdapter)
        context.system.receptionist ! Receptionist.Subscribe(PlayerActor.Service, listingAdapter)
        context.system.receptionist ! Receptionist.Subscribe(EatingManager.Service, listingAdapter)
        context.system.receptionist ! Receptionist.Subscribe(EndGameManager.Service, listingAdapter)
        Behaviors.withTimers: timers =>
            timers.startTimerAtFixedRate(Tick(), 60.milliseconds)
            WorldImpl(World(width, height, List.empty, initialFoods(30, width, height)), ctx = context).receive

  private def initialFoods(numFoods: Int, width: Int, height: Int, initialMass: Double = 100.0): Seq[Food] =
    (1 to numFoods).map(i => Food(s"f$i", Position(Random.nextInt(width), Random.nextInt(height)), initialMass))

  private case class WorldImpl(private var world: World, ctx: ActorContext[WorldMessage | Receptionist.Listing]):
    private var players: Seq[ActorRef[PlayerMessage]] = List.empty
    private var playerViews: Seq[ActorRef[PlayerViewMessage]] = List.empty
    private var eatingManager: Option[ActorRef[EatingManagerMessage]] = Option.empty
    private var endGameManager: Option[ActorRef[EndGameManagerMessage]] = Option.empty
    private var globalView: Option[ActorRef[GlobalViewMessage]] = Option.empty
    private var counter = 0

    val receive: Behavior[WorldMessage | Receptionist.Listing] = Behaviors.receiveMessagePartial:
      case msg: Receptionist.Listing =>
        msg.key match
          case PlayerView.Service =>
            ctx.log.info(s"LISTING VIEWS: ${msg.serviceInstances(PlayerView.Service).toList}")
            val viewServices = msg.serviceInstances(PlayerView.Service).toList
            if viewServices != playerViews then playerViews = viewServices
          case GlobalView.Service =>
            if msg.serviceInstances(GlobalView.Service).toList.nonEmpty then
              val globalViewService = msg.serviceInstances(GlobalView.Service).toList.head
              if !globalView.contains(globalViewService) then globalView = Some(globalViewService)
              ctx.log.info(s"GLOBAL VIEW: ${globalView.get}")
          case PlayerActor.Service =>
            ctx.log.info(s"LISTING PLAYERS: ${msg.serviceInstances(PlayerActor.Service).toList}")
            val playerServices = msg.serviceInstances(PlayerActor.Service).toList
            if playerServices != players then players = playerServices
          case EatingManager.Service =>
            if msg.serviceInstances(EatingManager.Service).toList.nonEmpty then
              val foodManagerService = msg.serviceInstances(EatingManager.Service).toList.head
              if !eatingManager.contains(foodManagerService) then eatingManager = Some(foodManagerService)
              ctx.log.info(s"FOOD MANAGER: ${eatingManager.get}")
          case EndGameManager.Service =>
            if msg.serviceInstances(EndGameManager.Service).toList.nonEmpty then
              val endGameManagerService = msg.serviceInstances(EndGameManager.Service).toList.head
              if !endGameManager.contains(endGameManagerService) then endGameManager = Some(endGameManagerService)
              ctx.log.info(s"END GAME MANAGER: ${endGameManager.get}")
        Behaviors.same
      case Tick() =>
        if players.nonEmpty then
          players.foreach(_ ! PlayerMessage.Ask(ctx.self))
          waitingValues
        else
          Behaviors.same
      case UpdateWorld() =>
        if eatingManager.nonEmpty then eatingManager.get ! EatingManagerMessage.UpdateWorld(world, ctx.self)
        Behaviors.same
      case UpdatedWorld(newWorld) =>
        newWorld.findModifiedPlayers.foreach(player => player.findActorById ! Grow(player))
        findEatenPlayersView(newWorld).foreach(_ ! PlayerViewMessage.Stop())
        players = newWorld.findAlivePlayersActor
        world = newWorld
        playerViews.foreach(_ ! PlayerViewMessage.RenderWorld(world))
        if globalView.nonEmpty then globalView.get ! GlobalViewMessage.RenderWorld(world)
        if endGameManager.nonEmpty then endGameManager.get ! EndGameManagerMessage.CheckEndGame(world, ctx.self)
        Behaviors.same
      case EndGame(winner) =>
        playerViews.foreach(_ ! PlayerViewMessage.EndGame(winner))
        ctx.system.terminate()
        Behaviors.stopped

    extension (player: Player)
      private def findActorById: ActorRef[PlayerMessage] = players.find(_.path.name == player.id).get
      private def findViewById: ActorRef[PlayerViewMessage] =
        val playerViewName = PlayerView.getClass.getSimpleName.dropRight(1)
        playerViews.find(_.path.name.drop(playerViewName.length) == player.id.drop(1)).get

    extension (newWorld: World)
      private def findModifiedPlayers: Seq[Player] = newWorld.players.filter(p => p != world.playerById(p.id))
      private def findAlivePlayersActor: Seq[ActorRef[PlayerMessage]] = newWorld.players.map(findActorById)

    private def findEatenPlayersView(newWorld: World): Seq[ActorRef[PlayerViewMessage]] =
      world.players.filter(p => newWorld.playerById(p.id).isEmpty).map(findViewById)

    private val waitingValues: Behavior[WorldMessage | Receptionist.Listing] =
      Behaviors.withStash[WorldMessage | Receptionist.Listing](1000): stash =>
        Behaviors.receiveMessagePartial:
          case SendPlayer(player, from) =>
            counter += 1
            world = if world.playerById(player.id).isEmpty then world.copy(players = player +: world.players)
              else world.updatePlayer(player)
            if counter == players.size then
              counter = 0
              stash.stash(UpdateWorld())
              stash.unstashAll(receive)
            else
              waitingValues
          case message =>
            stash.stash(message)
            Behaviors.same
