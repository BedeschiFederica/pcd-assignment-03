package pcd.ass03.model

import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, StashBuffer}
import akka.actor.typed.{ActorRef, Behavior}
import pcd.ass03.Message
import pcd.ass03.model.FoodManager.FoodManagerMessage
import pcd.ass03.model.World.WorldMessage
import pcd.ass03.view.PlayerView
import pcd.ass03.view.PlayerView.PlayerViewMessage

import scala.util.Random
import concurrent.duration.DurationInt

case class Position(x: Double, y: Double)

sealed trait Entity:
  def id: String
  def mass: Double
  def pos: Position
  def radius: Double = math.sqrt(mass / math.Pi)
  def distanceTo(other: Entity): Double = math.hypot(pos.x - other.pos.x, pos.y - other.pos.y)

object Player:
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
           (using random: Random): Behavior[PlayerMessage | Receptionist.Listing] =
    Behaviors.setup:
      context =>
        context.system.receptionist ! Receptionist.Register(Service, context.self)
        val listingAdapter: ActorRef[Receptionist.Listing] = context.messageAdapter(listing => listing)
        context.system.receptionist ! Receptionist.Subscribe(PlayerView.Service, listingAdapter)
        PlayerImpl(id, pos, mass, context).receive

  private case class PlayerImpl(override val id: String, private var _pos: Position, private var _mass: Double,
                                ctx: ActorContext[PlayerMessage | Receptionist.Listing])
                               (using random: Random) extends Entity:
    private val Speed = 10.0
    private val playerViewName = PlayerView.getClass.getSimpleName.dropRight(1)
    private var playerView: Option[ActorRef[PlayerViewMessage]] = Option.empty

    override def pos: Position = _pos

    override def mass: Double = _mass

    val receive: Behavior[PlayerMessage | Receptionist.Listing] = Behaviors.receiveMessagePartial:
      case msg: Receptionist.Listing =>
        //ctx.log.info(s"New frontend! $msg, ${}")
        val service = msg.serviceInstances(PlayerView.Service).toList.find(_.path.name == s"$playerViewName$id")
        if service.nonEmpty && playerView != service then
          playerView = service
          playerView.get ! PlayerViewMessage.Render(pos, radius, ctx.self)
        Behaviors.same
      case Move(dx, dy) =>
        //ctx.log.info(s"PlayerView: $playerView")
        _pos = Position((pos.x + dx * Speed).max(0).min(400), (pos.y + dy * Speed).max(0).min(400))
        //ctx.log.info(s"move to $pos}, ${ctx.self.path}")
        if playerView.nonEmpty then playerView.get ! PlayerViewMessage.Render(pos, radius, ctx.self)
        Behaviors.same
      case Stop() => Behaviors.stopped
      case Ask(from) =>
        ctx.log.info("RECEIVED ASK")
        from ! SendPlayer(_pos, radius, ctx.self)
        Behaviors.same
      case UpdatePos(newPos) =>
        _pos = newPos
        Behaviors.same

case class Food(id: String, pos: Position, mass: Double = 100.0) extends Entity

object FoodManager:
  import World.WorldMessage
  import WorldMessage.*

  trait FoodManagerMessage extends Message
  object FoodManagerMessage:
    case class Ask(from: ActorRef[WorldMessage]) extends FoodManagerMessage
    case class Stop() extends FoodManagerMessage

  import FoodManagerMessage.*

  val Service: ServiceKey[FoodManagerMessage] = ServiceKey[FoodManagerMessage]("FoodManagerService")
  def apply(): Behavior[FoodManagerMessage] =
    Behaviors.setup:
      context =>
        context.system.receptionist ! Receptionist.Register(Service, context.self)
        FoodManagerImpl(context).receive

  private case class FoodManagerImpl(ctx: ActorContext[FoodManagerMessage]):
    private var foods: List[Food] =
      (1 to 8).map(i => Food(s"f$i", Position(Random.nextInt(400), Random.nextInt(400)))).toList

    val receive: Behavior[FoodManagerMessage] = Behaviors.receiveMessagePartial:
      case FoodManagerMessage.Ask(from) =>
        from ! SendFood(foods)
        Behaviors.same
      case Stop() => Behaviors.stopped

/*case class World(
    width: Int,
    height: Int,
    players: Seq[Player],
    foods: Seq[Food]
  ):
  def playersExcludingSelf(player: Player): Seq[Player] =
    players.filterNot(_.id == player.id)

  def playerById(id: String): Option[Player] =
    players.find(_.id == id)

  def updatePlayer(player: Player): World =
    copy(players = players.map(p => if (p.id == player.id) player else p))

  def removePlayers(ids: Seq[Player]): World =
    copy(players = players.filterNot(p => ids.map(_.id).contains(p.id)))

  def removeFoods(ids: Seq[Food]): World =
    copy(foods = foods.filterNot(f => ids.contains(f)))*/

object World:
  import Player.PlayerMessage
  import PlayerMessage.*
  import pcd.ass03.view.PlayerView.PlayerViewMessage
  import PlayerViewMessage.RenderAll

  trait WorldMessage extends Message
  object WorldMessage:
    case class UpdatePlayer(oldPos: Position, dir: (Double, Double)) extends WorldMessage
    case class RemovePlayers() extends WorldMessage
    case class RemoveFoods() extends WorldMessage
    case class SendPlayer(pos: Position, radius: Double, from: ActorRef[PlayerMessage]) extends WorldMessage
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
        context.system.receptionist ! Receptionist.Subscribe(Player.Service, listingAdapter)
        context.system.receptionist ! Receptionist.Subscribe(FoodManager.Service, listingAdapter)
        Behaviors.withTimers: timers =>
            timers.startTimerAtFixedRate(Tick(), 60.milliseconds)
            WorldImpl(width, height, ctx = context).receive

  private case class WorldImpl(width: Int, height: Int, ctx: ActorContext[WorldMessage | Receptionist.Listing]):
    private var players: Seq[ActorRef[PlayerMessage]] = List.empty
    private var playerViews: Seq[ActorRef[PlayerViewMessage]] = List.empty
    private var playersValues: Map[ActorRef[PlayerMessage], (Position, Double)] = Map.empty
    private var foodManager: Option[ActorRef[FoodManagerMessage]] = Option.empty
    private var foods: List[Food] = List.empty
    private var counter = 0
    private var foodUpdated = false

    val receive: Behavior[WorldMessage | Receptionist.Listing] = Behaviors.receiveMessagePartial:
      case msg: Receptionist.Listing =>
        msg.key match
          case PlayerView.Service =>
            ctx.log.info(s"LISTING VIEWS: ${msg.serviceInstances(PlayerView.Service).toList}")
            val viewServices = msg.serviceInstances(PlayerView.Service).toList
            if viewServices != playerViews then playerViews = viewServices
          case Player.Service =>
            ctx.log.info(s"LISTING PLAYERS: ${msg.serviceInstances(Player.Service).toList}")
            val playerServices = msg.serviceInstances(Player.Service).toList
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
        ctx.log.info(s"RECEIVED UPDATE WORLD, map: $playersValues, foods: $foods")
        playerViews.foreach(_ ! RenderAll(playersValues.map((player, values) => (player.path.name, values)),
          foods.map(food => (food.id, (food.pos, food.radius))).toMap))
        Behaviors.same

    private val waitingValues: Behavior[WorldMessage | Receptionist.Listing] =
      Behaviors.withStash[WorldMessage | Receptionist.Listing](1000): stash =>
        Behaviors.receiveMessagePartial:
          case SendPlayer(pos, radius, from) =>
            counter += 1
            playersValues = playersValues + (from -> (pos, radius))
            checkReceivedAllValues(stash)
          case SendFood(foodsList) =>
            foods = foodsList
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

    /*def playersExcludingSelf(player: Player): Seq[Player] =
      players.filterNot(_.id == player.id)

    def playerById(id: String): Option[Player] =
      players.find(_.id == id)

    def updatePlayer(player: Player): World =
      copy(players = players.map(p => if (p.id == player.id) player else p))

    def removePlayers(ids: Seq[Player]): World =
      copy(players = players.filterNot(p => ids.map(_.id).contains(p.id)))

    def removeFoods(ids: Seq[Food]): World =
      copy(foods = foods.filterNot(f => ids.contains(f)))*/
