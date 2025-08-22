package pcd.ass03.model

import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, StashBuffer}
import akka.actor.typed.{ActorRef, Behavior}
import pcd.ass03.Message
import pcd.ass03.model.FoodManager.FoodManagerMessage
import pcd.ass03.model.WorldManager.WorldMessage
import pcd.ass03.view.PlayerView
import pcd.ass03.view.PlayerView.PlayerViewMessage

import scala.util.Random
import concurrent.duration.DurationInt

object PlayerActor:
  import WorldMessage.*

  trait PlayerMessage extends Message
  object PlayerMessage:
    case class Move(dx: Double, dy: Double) extends PlayerMessage
    case class Ask(from: ActorRef[WorldMessage]) extends PlayerMessage
    case class Stop() extends PlayerMessage
    case class Grow(entity: Entity) extends PlayerMessage

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
        //ctx.log.info(s"New frontend! $msg, ${}")
        val service = msg.serviceInstances(PlayerView.Service).toList
          .find(_.path.name == s"$playerViewName${player.id.drop(1)}")
        if service.nonEmpty && playerView != service then
          playerView = service
          playerView.get ! PlayerViewMessage.Render(player, ctx.self)
        Behaviors.same
      case Move(dx, dy) =>
        //ctx.log.info(s"PlayerView: $playerView")
        player = player.copy(pos = Position((player.pos.x + dx * Speed).max(0).min(width),
          (player.pos.y + dy * Speed).max(0).min(height)))
        //ctx.log.info(s"move to $pos}, ${ctx.self.path}")
        if playerView.nonEmpty then playerView.get ! PlayerViewMessage.Render(player, ctx.self)
        Behaviors.same
      case Stop() => Behaviors.stopped
      case Ask(from) =>
        ctx.log.info("RECEIVED ASK")
        from ! SendPlayer(player, ctx.self)
        Behaviors.same
      case UpdatePos(newPos) =>
        player = player.copy(pos = newPos)
        Behaviors.same

object FoodManager:
  import WorldManager.WorldMessage
  import WorldMessage.*

  trait FoodManagerMessage extends Message
  object FoodManagerMessage:
    case class Ask(from: ActorRef[WorldMessage]) extends FoodManagerMessage
    case class Stop() extends FoodManagerMessage

  import FoodManagerMessage.*

  val Service: ServiceKey[FoodManagerMessage] = ServiceKey[FoodManagerMessage]("FoodManagerService")
  def apply(width: Int, height: Int): Behavior[FoodManagerMessage] =
    Behaviors.setup:
      context =>
        context.system.receptionist ! Receptionist.Register(Service, context.self)
        FoodManagerImpl(context)(width, height).receive

  private case class FoodManagerImpl(ctx: ActorContext[FoodManagerMessage])(width: Int, height: Int):
    private var foods: List[Food] =
      (1 to 8).map(i => Food(s"f$i", Position(Random.nextInt(width), Random.nextInt(height)))).toList

    val receive: Behavior[FoodManagerMessage] = Behaviors.receiveMessagePartial:
      case FoodManagerMessage.Ask(from) =>
        from ! SendFood(foods)
        Behaviors.same
      case Stop() => Behaviors.stopped

object WorldManager:
  import PlayerActor.PlayerMessage
  import PlayerMessage.*
  import pcd.ass03.view.PlayerView.PlayerViewMessage
  import PlayerViewMessage.RenderWorld

  trait WorldMessage extends Message
  object WorldMessage:
    case class SendPlayer(player: Player, from: ActorRef[PlayerMessage]) extends WorldMessage
    case class SendFood(foods: List[Food]) extends WorldMessage
    case class Tick() extends WorldMessage
    case class UpdateWorld() extends WorldMessage

  import WorldMessage.*

  val Service: ServiceKey[WorldMessage] = ServiceKey[WorldMessage]("WorldService")
  def apply(width: Int, height: Int): Behavior[WorldMessage | Receptionist.Listing] =
    Behaviors.setup:
      context =>
        context.system.receptionist ! Receptionist.Register(Service, context.self)
        val listingAdapter: ActorRef[Receptionist.Listing] = context.messageAdapter(listing => listing)
        context.system.receptionist ! Receptionist.Subscribe(PlayerView.Service, listingAdapter)
        context.system.receptionist ! Receptionist.Subscribe(PlayerActor.Service, listingAdapter)
        context.system.receptionist ! Receptionist.Subscribe(FoodManager.Service, listingAdapter)
        Behaviors.withTimers: timers =>
            timers.startTimerAtFixedRate(Tick(), 60.milliseconds)
            WorldImpl(World(width, height, List.empty, List.empty), ctx = context).receive

  private case class WorldImpl(private var world: World, ctx: ActorContext[WorldMessage | Receptionist.Listing]):
    private var players: Seq[ActorRef[PlayerMessage]] = List.empty
    private var playerViews: Seq[ActorRef[PlayerViewMessage]] = List.empty
    private var foodManager: Option[ActorRef[FoodManagerMessage]] = Option.empty
    private var counter = 0
    private var foodUpdated = false

    val receive: Behavior[WorldMessage | Receptionist.Listing] = Behaviors.receiveMessagePartial:
      case msg: Receptionist.Listing =>
        msg.key match
          case PlayerView.Service =>
            ctx.log.info(s"LISTING VIEWS: ${msg.serviceInstances(PlayerView.Service).toList}")
            val viewServices = msg.serviceInstances(PlayerView.Service).toList
            if viewServices != playerViews then playerViews = viewServices
          case PlayerActor.Service =>
            ctx.log.info(s"LISTING PLAYERS: ${msg.serviceInstances(PlayerActor.Service).toList}")
            val playerServices = msg.serviceInstances(PlayerActor.Service).toList
            if playerServices != players then players = playerServices
          case FoodManager.Service =>
            if msg.serviceInstances(FoodManager.Service).toList.nonEmpty then
              val foodManagerService = msg.serviceInstances(FoodManager.Service).toList.head
              if !foodManager.contains(foodManagerService) then foodManager = Some(foodManagerService)
              ctx.log.info(s"FOOD MANAGER: ${foodManager.get}")
        Behaviors.same
      case Tick() =>
        ctx.log.info(s"TICK")
        if players.nonEmpty && foodManager.nonEmpty then
          players.foreach(_ ! PlayerMessage.Ask(ctx.self))
          foodManager.get ! FoodManagerMessage.Ask(ctx.self)
          waitingValues
        else
          Behaviors.same
      case UpdateWorld() =>
        ctx.log.info(s"RECEIVED UPDATE WORLD, world: $world")
        playerViews.foreach(_ ! RenderWorld(world))
        Behaviors.same

    private val waitingValues: Behavior[WorldMessage | Receptionist.Listing] =
      Behaviors.withStash[WorldMessage | Receptionist.Listing](1000): stash =>
        Behaviors.receiveMessagePartial:
          case SendPlayer(player, from) =>
            counter += 1
            world = if world.playerById(player.id).isEmpty then world.copy(players = player +: world.players)
              else world.updatePlayer(player)
            checkReceivedAllValues(stash)
          case SendFood(foodsList) =>
            world = world.copy(foods = foodsList)
            foodUpdated = true
            checkReceivedAllValues(stash)
          case message =>
            stash.stash(message)
            Behaviors.same

    private def checkReceivedAllValues(
                                        stash: StashBuffer[WorldMessage | Receptionist.Listing]
                                      ): Behavior[WorldMessage | Receptionist.Listing] =
      if counter == players.size && foodUpdated then
        counter = 0
        foodUpdated = false
        stash.stash(UpdateWorld())
        stash.unstashAll(receive)
      else
        waitingValues
