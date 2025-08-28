package pcd.ass03.model

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import pcd.ass03.Message
import pcd.ass03.model.WorldManager.{DataKey, WorldKey, WorldMessage}
import pcd.ass03.view.PlayerView
import PlayerView.PlayerViewMessage
import WorldMessage.*
import akka.cluster.ddata.LWWMap
import akka.cluster.ddata.typed.scaladsl.{DistributedData, Replicator}
import akka.cluster.ddata.typed.scaladsl.Replicator.{Get, GetResponse, ReadLocal}

object PlayerActor:

  trait PlayerMessage extends Message
  object PlayerMessage:
    case class Move(dx: Double, dy: Double) extends PlayerMessage
    case class Ask(world: World, from: ActorRef[WorldMessage]) extends PlayerMessage
    case class Grow(player: Player) extends PlayerMessage
    case class Stop() extends PlayerMessage
    case class InternalGetResponse(rsp: GetResponse[LWWMap[String, World]]) extends PlayerMessage

  import PlayerMessage.*

  val Service: ServiceKey[PlayerMessage] = ServiceKey[PlayerMessage]("PlayerService")
  def apply(id: String, pos: Position, mass: Double)(ai: Boolean = false)
           (width: Int, height: Int): Behavior[PlayerMessage | Receptionist.Listing] =
    Behaviors.setup: context =>
      val replicator = DistributedData(context.system).replicator
      replicator ! Get(DataKey, ReadLocal, context.messageAdapter(InternalGetResponse.apply))

      context.system.receptionist ! Receptionist.Register(Service, context.self)
      val listingAdapter: ActorRef[Receptionist.Listing] = context.messageAdapter(listing => listing)
      context.system.receptionist ! Receptionist.Subscribe(PlayerView.Service, listingAdapter)
      PlayerImpl(Player(id, pos, mass), ai, context)(width, height).receive

  private case class PlayerImpl(private var player: Player, ai: Boolean,
                                ctx: ActorContext[PlayerMessage | Receptionist.Listing])(width: Int, height: Int):
    private val playerViewName = PlayerView.getClass.getSimpleName.dropRight(1)
    private var playerView: Option[ActorRef[PlayerViewMessage]] = Option.empty

    val receive: Behavior[PlayerMessage | Receptionist.Listing] = Behaviors.receiveMessagePartial:
      case InternalGetResponse(rsp) => rsp match
        case Replicator.GetSuccess(`DataKey`) =>
          val data: LWWMap[String, World] = rsp.asInstanceOf[Replicator.GetSuccess[LWWMap[String, World]]].get(DataKey)
          data.get(WorldKey).foreach(_.playerById(player.id).foreach(player = _))
        case _ => ()
        Behaviors.same
      case msg: Receptionist.Listing =>
        val service = msg.serviceInstances(PlayerView.Service).toList
          .find(_.path.name == s"$playerViewName${player.id.drop(1)}")
        if service.nonEmpty && playerView != service then
          playerView = service
          playerView.get ! PlayerViewMessage.Render(player, ctx.self)
        Behaviors.same
      case Move(dx, dy) =>
        if !ai then updateAndRenderPlayer((dx, dy))
        Behaviors.same
      case Ask(world, from) =>
        if ai then updateAndRenderPlayer(AIMovement.moveAI(player.id, world))
        from ! SendPlayer(player, ctx.self)
        Behaviors.same
      case Grow(newPlayer) =>
        player = newPlayer
        Behaviors.same
      case Stop() => Behaviors.stopped

    private def updateAndRenderPlayer(dir: (Double, Double)): Unit =
      player = player.updatePosition(dir, (width, height))
      playerView.foreach(_ ! PlayerViewMessage.Render(player, ctx.self))
