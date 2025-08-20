package pcd.ass03.model

import akka.actor.typed.receptionist.Receptionist
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

  trait PlayerMessage extends Message
  private object PlayerMessage:
    case class Move() extends PlayerMessage
    case class Stop() extends PlayerMessage
    case class Grow(entity: Entity) extends PlayerMessage

  import PlayerMessage.*

  def apply(id: String, pos: Position, mass: Double)
           (using random: Random): Behavior[PlayerMessage | Receptionist.Listing] =
    Behaviors.setup:
      context =>
        context.system.receptionist ! Receptionist.Subscribe(PlayerView.Service, context.self)
        Behaviors.withTimers:
          timers =>
            timers.startTimerAtFixedRate(Move(), 60.milliseconds)
            PlayerImpl(id, pos, mass, context).receive

  private case class PlayerImpl(override val id: String, private var _pos: Position, private var _mass: Double,
                                ctx: ActorContext[PlayerMessage | Receptionist.Listing],
                                private var frontends: List[ActorRef[PlayerViewMessage.Render]] = List.empty)
                               (using random: Random) extends Player:
    override def pos: Position = _pos

    override def mass: Double = _mass

    override def copy(id: String, pos: Position, mass: Double): Player = copy(id, pos, mass)

    val receive: Behavior[PlayerMessage | Receptionist.Listing] = Behaviors.receiveMessagePartial:
      case msg: Receptionist.Listing =>
        ctx.log.info(s"New frontend! $msg")
        val services = msg.serviceInstances(PlayerView.Service).toList
        if (services == frontends)
          Behaviors.same
        else
          frontends = msg.serviceInstances(PlayerView.Service).toList
          receive
      case Move() =>
        val (deltaX, deltaY) = ((random.nextGaussian * 5).toInt, (random.nextGaussian * 5).toInt)
        frontends.foreach(_ ! PlayerViewMessage.Render(pos, radius, ctx.self))
        ctx.log.info(s"move from $pos}, ${ctx.self.path}")
        _pos = Position(pos.x + deltaX, pos.y + deltaY)
        receive
      case Stop() => Behaviors.stopped

sealed trait Food extends Entity
object Food:

  trait FoodMessage extends Message
  private object FoodMessage:
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
      case Stop() => Behaviors.stopped

object World:

  trait WorldMessage
  object WorldMessage:
    case class UpdatePlayer() extends WorldMessage

case class World(
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
    copy(foods = foods.filterNot(f => ids.contains(f)))
