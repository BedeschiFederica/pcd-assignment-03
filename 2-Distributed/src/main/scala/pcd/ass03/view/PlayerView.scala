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
  import PlayerMessage.Move

  trait PlayerViewMessage extends Message
  object PlayerViewMessage:
    case class Render(player: Player, from: ActorRef[PlayerMessage]) extends PlayerViewMessage
    case class RenderWorld(world: World) extends PlayerViewMessage
    case class Flush() extends PlayerViewMessage
    case class UpdatePlayer(dx: Double, dy: Double) extends PlayerViewMessage
    case class EndGame(winner: Player) extends PlayerViewMessage
    case class Stop() extends PlayerViewMessage

  import PlayerViewMessage.*
  import AgarViewUtils.*

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
            worldActor = Option(msg.serviceInstances(WorldManager.Service).toList).collect:
              case l if l.nonEmpty => l.head
            Behaviors.same
          case Render(player, from) =>
            if playerActor.isEmpty then playerActor = Some(from)
            world = world.fold(Some(World(width, height, List(player), List.empty)))
              (_ => Some(world.get.updatePlayer(player)))
            update()
            Behaviors.same
          case RenderWorld(newWorld) =>
            world = Some(newWorld)
            update()
            Behaviors.same
          case Flush() =>
            update()
            Behaviors.same
          case UpdatePlayer(dx, dy) =>
            playerActor.foreach(_ ! Move(dx, dy))
            Behaviors.same
          case Stop() =>
            frame.close()
            Behaviors.stopped
          case EndGame(winner) =>
            Dialog.showConfirmation(frame, s"The winner is Player ${winner.id.drop(1)}", "End game",
              Dialog.Options.Default) match
              case _ =>
                frame.close()
                ctx.system.terminate()
            Behaviors.stopped

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
          drawWorld(g, world.get, offsetX, offsetY)

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
