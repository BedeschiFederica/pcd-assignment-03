package pcd.ass03.model

import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import pcd.ass03.Message
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

sealed trait Player extends Entity:
  def copy(id: String, pos: Position, mass: Double): Player
  def grow(entity: Entity): Player = this

object Player:
  import World.WorldMessage
  import WorldMessage.*

  trait PlayerMessage extends Message
  object PlayerMessage:
    case class Move(dx: Double, dy: Double) extends PlayerMessage
    case class Stop() extends PlayerMessage
    case class Grow(entity: Entity) extends PlayerMessage
    case class Ask(from: ActorRef[WorldMessage]) extends PlayerMessage
    case class UpdatePos(pos: Position) extends PlayerMessage

  import PlayerMessage.*

  def apply(id: String, pos: Position, mass: Double)
           (using random: Random): Behavior[PlayerMessage | Receptionist.Listing] =
    Behaviors.setup:
      context =>
        context.system.receptionist ! Receptionist.Subscribe(PlayerView.Service, context.self)
        /*Behaviors.withTimers:
          timers =>
            timers.startTimerAtFixedRate(Move(), 60.milliseconds)*/
        PlayerImpl(id, pos, mass, context).receive

  private case class PlayerImpl(override val id: String, private var _pos: Position, private var _mass: Double,
                                ctx: ActorContext[PlayerMessage | Receptionist.Listing])
                               (using random: Random) extends Player:
    private var playerView: Option[ActorRef[PlayerViewMessage]] = Option.empty

    override def pos: Position = _pos

    override def mass: Double = _mass

    override def copy(id: String, pos: Position, mass: Double): Player = copy(id, pos, mass)

    val receive: Behavior[PlayerMessage | Receptionist.Listing] = Behaviors.receiveMessagePartial:
      case msg: Receptionist.Listing =>
        ctx.log.info(s"New frontend! $msg")
        val service = msg.serviceInstances(PlayerView.Service).toList.find(_.path.name == id)
        if service.nonEmpty && playerView != service then
          playerView = service
          playerView.get ! PlayerViewMessage.Render(pos, radius, ctx.self)
        Behaviors.same
      case Move(dx, dy) =>
        //ctx.log.info(s"PlayerView: $playerView")
        _pos = Position(pos.x + dx, pos.y + dy)
        ctx.log.info(s"move to $pos}, ${ctx.self.path}")
        if playerView.nonEmpty then playerView.get ! PlayerViewMessage.Render(pos, radius, ctx.self)
        Behaviors.same
      case Stop() => Behaviors.stopped
      case Ask(from) =>
        ctx.log.info("RECEIVED ASK")
        from ! Send(_pos, ctx.self)
        Behaviors.same
      case UpdatePos(newPos) =>
        _pos = newPos
        Behaviors.same

sealed trait Food extends Entity
object Food:
  def apply(id: String, pos: Position, mass: Double = 100.0): Food = FoodImpl(id, pos, mass)

  private case class FoodImpl(id: String, pos: Position, mass: Double = 100.0) extends Food

/*trait FoodMessage extends Message
object FoodMessage:
  case class Stop() extends FoodMessage

import FoodMessage.*

def apply(id: String, pos: Position, mass: Double = 100.0): Behavior[FoodMessage | Receptionist.Listing] =
  Behaviors.setup:
    context =>
      context.system.receptionist ! Receptionist.Subscribe(PlayerView.Service, context.self)
      FoodImpl(id, pos, mass, context).receive

private case class FoodImpl(override val id: String, override val pos: Position, override val mass: Double,
                              ctx: ActorContext[FoodMessage | Receptionist.Listing],
                              private var frontends: List[ActorRef[PlayerViewMessage.Render]] = List.empty)
                             extends Food:

  val receive: Behavior[FoodMessage | Receptionist.Listing] = Behaviors.receiveMessagePartial:
    case msg: Receptionist.Listing =>
      ctx.log.info(s"New frontend! $msg")
      val services = msg.serviceInstances(PlayerView.Service).toList
      if (services == frontends)
        Behaviors.same
      else
        frontends = msg.serviceInstances(PlayerView.Service).toList
        receive
    case Stop() => Behaviors.stopped*/

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

  trait WorldMessage extends Message
  object WorldMessage:
    case class UpdatePlayer(oldPos: Position, dir: (Double, Double)) extends WorldMessage
    case class RemovePlayers() extends WorldMessage
    case class RemoveFoods() extends WorldMessage
    case class Send(pos: Position, from: ActorRef[PlayerMessage]) extends WorldMessage
    case class UpdateWorld() extends WorldMessage

  import WorldMessage.*

  val Service: ServiceKey[WorldMessage] = ServiceKey[WorldMessage]("WorldService")
  def apply(width: Int, height: Int): Behavior[WorldMessage | Receptionist.Listing] =
    Behaviors.setup:
      context =>
        context.system.receptionist ! Receptionist.Register(Service, context.self)
        /*val listingAdapter: ActorRef[Receptionist.Listing] = ctx.messageAdapter(listing => listing)
        ctx.system.receptionist ! Receptionist.Subscribe(World.Service, listingAdapter)*/
        //context.system.receptionist ! Receptionist.Subscribe(PlayerView.Service, context.self)
        Behaviors.withTimers: timers =>
            timers.startTimerAtFixedRate(UpdateWorld(), 60.milliseconds)
            WorldImpl(width, height, ctx = context).receive

  private case class WorldImpl(width: Int, height: Int, players: Seq[ActorRef[PlayerMessage]] = List.empty,
                               foods: Seq[Food] = List.empty, ctx: ActorContext[WorldMessage | Receptionist.Listing]):
    private val Speed = 10.0
    private var playerViews: List[ActorRef[PlayerViewMessage]] = List.empty

    val receive: Behavior[WorldMessage | Receptionist.Listing] = Behaviors.receiveMessagePartial:
      /*case msg: Receptionist.Listing =>
        ctx.log.info(s"New frontend! $msg")
        val services = msg.serviceInstances(PlayerView.Service).toList
        if services != playerViews then playerViews = services
        Behaviors.same
      case UpdateWorld() =>
        playerViews.foreach(_ ! RenderAll(players.map(p => )))
        Behaviors.same*/
      case UpdatePlayer(oldPos, dir) =>
        ctx.log.info(s"UPDATE PLAYER, players: $players")
        players.foreach(_ ! Ask(ctx.self))
        waitingPos(oldPos, dir)

    private val waitingPos: (Position, (Double, Double)) => Behavior[WorldMessage | Receptionist.Listing] =
      (posToSearch, dir) => Behaviors.withStash[WorldMessage | Receptionist.Listing](1000): stash =>
        Behaviors.receiveMessagePartial:
          case Send(`posToSearch`, from) =>
            val newX = (posToSearch.x + dir._1 * Speed).max(0).min(width)
            val newY = (posToSearch.y + dir._2 * Speed).max(0).min(height)
            from ! UpdatePos(Position(newX, newY))
            stash.unstashAll(receive)
          case message =>
            stash.stash(message)
            Behaviors.same

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
