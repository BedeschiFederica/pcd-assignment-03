package pcd.ass03.view

import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import pcd.ass03.Message
import pcd.ass03.model.WorldManager.WorldMessage
import pcd.ass03.model.{Player, World, WorldManager}

import java.awt.Graphics2D
import javax.swing.SwingUtilities
import scala.concurrent.Future
import scala.swing.*
import scala.concurrent.duration.DurationInt
import scala.util.Success

object PlayerView:
  import pcd.ass03.model.PlayerActor.PlayerMessage
  import PlayerMessage.*

  trait PlayerViewMessage extends Message
  object PlayerViewMessage:
    case class Render(player: Player, from: ActorRef[PlayerMessage]) extends PlayerViewMessage
    case class RenderWorld(world: World) extends PlayerViewMessage
    case class Flush() extends PlayerViewMessage
    case class UpdatePlayer(dx: Double, dy: Double) extends PlayerViewMessage

  import PlayerViewMessage.*
  export AgarViewUtils.*

  val Service: ServiceKey[PlayerViewMessage] = ServiceKey[PlayerViewMessage]("RenderService")
  def apply(width: Int, height: Int)(frameRate: Double = 60): Behavior[PlayerViewMessage | Receptionist.Listing] =
    Behaviors.setup: ctx =>
      ctx.system.receptionist ! Receptionist.Register(Service, ctx.self)
      val listingAdapter: ActorRef[Receptionist.Listing] = ctx.messageAdapter(listing => listing)
      ctx.system.receptionist ! Receptionist.Subscribe(WorldManager.Service, listingAdapter)
      PlayerViewImpl(width, height)(frameRate).setup

  private case class PlayerViewImpl(width: Int, height: Int)(frameRate: Double):
    private var playerActor: Option[ActorRef[PlayerMessage]] = Option.empty
    private var worldActor: Option[ActorRef[WorldMessage]] = Option.empty
    private var world: Option[World] = Option.empty

    val setup: Behavior[PlayerViewMessage | Receptionist.Listing] = Behaviors.setup: ctx =>
      panel.addListener(ctx)
      receive

    private val receive: Behavior[PlayerViewMessage | Receptionist.Listing] = Behaviors.setup: ctx =>
      Behaviors.withTimers: timers =>
        timers.startTimerAtFixedRate(Flush(), ((1 / frameRate) * 1000).toInt.milliseconds)
        Behaviors.receiveMessagePartial:
          case msg: Receptionist.Listing =>
            //ctx.log.info(s"LISTING $msg ${msg.serviceInstances(World.Service).toList}")
            if msg.serviceInstances(WorldManager.Service).toList.nonEmpty then
              val service = msg.serviceInstances(WorldManager.Service).toList.head
              if !worldActor.contains(service) then worldActor = Some(service)
              ctx.log.info(s"NEW WORLD! ${worldActor.get}")
            Behaviors.same
          case Render(player, from) =>
            if playerActor.isEmpty then playerActor = Some(from)
            //ctx.log.info(s"RENDER PLAYER.. $id: $pos")
            if world.isEmpty then
              world = Some(World(width, height, List(player), List.empty))
            else
              world = Some(world.get.updatePlayer(player))
            update()
            Behaviors.same
          case RenderWorld(newWorld) =>
            ctx.log.info(s"RENDER WORLD, world: $newWorld")
            world = Some(newWorld)
            update()
            Behaviors.same
          case Flush() =>
            update()
            Behaviors.same
          case UpdatePlayer(dx, dy) =>
            //ctx.log.info("UPDATE PLAYER")
            playerActor.get ! Move(dx, dy)
            // repaint() ?
            Behaviors.same

    private val panel = new FlowPanel:
      listenTo(keys, mouse.moves)
      focusable = true
      requestFocusInWindow()

      override def paintComponent(g: Graphics2D): Unit =
        super.paintComponent(g)
        if world.nonEmpty && playerActor.nonEmpty then
          val playerOpt = world.get.players.find(_.id == playerActor.get.path.name)
          val (offsetX, offsetY) = playerOpt
            .map(p => (p.pos.x - size.width / 2.0, p.pos.y - size.height / 2.0))
            .getOrElse((0.0, 0.0))
          AgarViewUtils.drawWorld(g, world.get, offsetX, offsetY)

    private val frame = new MainFrame:
      title = s"Agar.io - Local View"
      preferredSize = new Dimension(400, 400)
      contents = panel
    frame.open()

    extension (panel: FlowPanel)
      private def addListener(context: ActorContext[PlayerViewMessage | Receptionist.Listing]): Unit =
        panel.reactions += { case e: event.MouseMoved =>
          val mousePos = e.point
          val dx = (mousePos.x - frame.size.width / 2) * 0.01
          val dy = (mousePos.y - frame.size.height / 2) * 0.01
          context.pipeToSelf(Future.successful(dx, dy)):
            case Success(_) => UpdatePlayer(dx, dy)
            case _ => throw IllegalStateException("Future unsuccessful.")
        }

    def update(): Unit = SwingUtilities.invokeLater(() => panel.repaint())
