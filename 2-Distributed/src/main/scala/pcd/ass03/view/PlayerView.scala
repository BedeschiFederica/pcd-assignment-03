package pcd.ass03.view

import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import pcd.ass03.Message
import pcd.ass03.model.World.WorldMessage
import pcd.ass03.model.{Position, World}

import java.awt.Graphics2D
import javax.swing.SwingUtilities
import scala.concurrent.Future
import scala.swing.*
import scala.concurrent.duration.DurationInt
import scala.util.Success

object PlayerView:
  import pcd.ass03.model.Player.PlayerMessage
  import PlayerMessage.*

  trait PlayerViewMessage extends Message
  object PlayerViewMessage:
    case class Render(pos: Position, radius: Double, id: ActorRef[PlayerMessage]) extends PlayerViewMessage
    case class RenderAll(toRender: Map[ActorRef[PlayerMessage], (Position, Double)]) extends PlayerViewMessage
    case class Flush() extends PlayerViewMessage
    case class UpdatePlayer(dx: Double, dy: Double) extends PlayerViewMessage

  import PlayerViewMessage.*

  val Service: ServiceKey[PlayerViewMessage] = ServiceKey[PlayerViewMessage]("RenderService")
  def apply(frameRate: Double = 60): Behavior[PlayerViewMessage | Receptionist.Listing] = Behaviors.setup: ctx =>
    ctx.system.receptionist ! Receptionist.Register(Service, ctx.self)
    val listingAdapter: ActorRef[Receptionist.Listing] = ctx.messageAdapter(listing => listing)
    ctx.system.receptionist ! Receptionist.Subscribe(World.Service, listingAdapter)
    PlayerViewImpl(frameRate).setup

  private case class PlayerViewImpl(frameRate: Double):
    private var playerActor: Option[ActorRef[PlayerMessage]] = Option.empty
    private var worldActor: Option[ActorRef[WorldMessage]] = Option.empty

    val setup: Behavior[PlayerViewMessage | Receptionist.Listing] = Behaviors.setup: ctx =>
      panel.addListener(ctx)
      receive

    private val receive: Behavior[PlayerViewMessage | Receptionist.Listing] = Behaviors.setup: ctx =>
      Behaviors.withTimers: timers =>
        timers.startTimerAtFixedRate(Flush(), ((1 / frameRate) * 1000).toInt.milliseconds)
        var toRender: Map[ActorRef[PlayerMessage], (Position, Double)] = Map.empty
        Behaviors.receiveMessagePartial:
          case msg: Receptionist.Listing =>
            //ctx.log.info(s"LISTING $msg ${msg.serviceInstances(World.Service).toList}")
            if msg.serviceInstances(World.Service).toList.nonEmpty then
              val service = msg.serviceInstances(World.Service).toList.head
              if !worldActor.contains(service) then worldActor = Some(service)
              ctx.log.info(s"NEW WORLD! ${worldActor.get}")
            Behaviors.same
          case Render(pos, radius, id) =>
            if playerActor.isEmpty then playerActor = Some(id)
            //ctx.log.info(s"render.. $id: $pos")
            toRender = toRender + (id -> (pos, radius))
            update(toRender.values.toList)
            Behaviors.same
          case RenderAll(map) =>
            toRender = map
            update(toRender.values.toList)
            Behaviors.same
          case Flush() =>
            update(toRender.values.toList)
            Behaviors.same
          case UpdatePlayer(dx, dy) =>
            ctx.log.info("UPDATE PLAYER")
            playerActor.get ! Move(dx, dy)
            // repaint() ?
            Behaviors.same

    private var entities: List[(Position, Double)] = List.empty
    private val panel = new FlowPanel:
      listenTo(keys, mouse.moves)
      focusable = true
      requestFocusInWindow()

      override def paintComponent(g: Graphics2D): Unit =
        super.paintComponent(g)
        entities.foreach:
          (pos, radius) =>
            val diameter = radius * 2
            //val (borderX, borderY) = toScreenCenter(pos, radius)
            g.setColor(java.awt.Color.blue)
            g.drawOval(pos.x.toInt, pos.y.toInt, diameter.toInt, diameter.toInt)
            g.fillOval(pos.x.toInt, pos.y.toInt, diameter.toInt, diameter.toInt)
        /*val world = manager.getWorld
        val playerOpt = world.players.find(_.id == playerId)
        val (offsetX, offsetY) = playerOpt
          .map(p => (p.pos.x - size.width / 2.0, p.pos.y - size.height / 2.0))
          .getOrElse((0.0, 0.0))
        AgarViewUtils.drawWorld(g, world, offsetX, offsetY)*/

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
          context.pipeToSelf(Future.successful(Position(mousePos.x, mousePos.y), (dx, dy))):
            case Success(_) => UpdatePlayer(dx, dy)
            case _ => throw IllegalStateException("Future unsuccessful.")
        }

    def update(values: List[(Position, Double)]): Unit = SwingUtilities.invokeLater: () =>
      entities = values
      panel.repaint()

    /*def toScreenCenter(pos: Position, radius: Double): (Int, Int) =
      val (offsetX, offsetY) = (pos.x - preferredSize.width / 2.0, pos.y - preferredSize.height / 2.0)
      ((pos.x - offsetX - radius).toInt, (pos.y - offsetY - radius).toInt)

    def toScreenLabel(x: Double, y: Double): (Int, Int) =
      ((x - offsetX - playerLabelOffsetX).toInt, (y - offsetY - playerLabelOffsetY).toInt)*/
