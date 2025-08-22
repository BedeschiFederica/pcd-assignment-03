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
    case class RenderAll(players: Map[String, (Position, Double)], foods: Map[String, (Position, Double)])
      extends PlayerViewMessage
    case class Flush() extends PlayerViewMessage
    case class UpdatePlayer(dx: Double, dy: Double) extends PlayerViewMessage

  import PlayerViewMessage.*
  export AgarViewUtils.*

  val Service: ServiceKey[PlayerViewMessage] = ServiceKey[PlayerViewMessage]("RenderService")
  def apply(frameRate: Double = 60): Behavior[PlayerViewMessage | Receptionist.Listing] = Behaviors.setup: ctx =>
    ctx.system.receptionist ! Receptionist.Register(Service, ctx.self)
    val listingAdapter: ActorRef[Receptionist.Listing] = ctx.messageAdapter(listing => listing)
    ctx.system.receptionist ! Receptionist.Subscribe(World.Service, listingAdapter)
    PlayerViewImpl(frameRate).setup

  private case class PlayerViewImpl(frameRate: Double):
    private var playerActor: Option[ActorRef[PlayerMessage]] = Option.empty
    private var worldActor: Option[ActorRef[WorldMessage]] = Option.empty
    private var toRender: (Map[String, (Position, Double)], Map[String, (Position, Double)]) = (Map.empty, Map.empty)

    val setup: Behavior[PlayerViewMessage | Receptionist.Listing] = Behaviors.setup: ctx =>
      panel.addListener(ctx)
      receive

    private val receive: Behavior[PlayerViewMessage | Receptionist.Listing] = Behaviors.setup: ctx =>
      Behaviors.withTimers: timers =>
        timers.startTimerAtFixedRate(Flush(), ((1 / frameRate) * 1000).toInt.milliseconds)
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
            ctx.log.info(s"RENDER PLAYER.. $id: $pos")
            toRender = toRender.copy(_1 = toRender._1 + (id.path.name -> (pos, radius)))
            update()
            Behaviors.same
          case RenderAll(players, foods) =>
            ctx.log.info(s"RENDER ALL, players: $players, foods: $foods")
            toRender = (players, foods)
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
        if playerActor.nonEmpty then
          val (offsetX, offsetY) = toRender._1.get(playerActor.get.path.name)
            .map((pos, _) => (pos.x - size.width / 2.0, pos.y - size.height / 2.0))
            .getOrElse((0.0, 0.0))
          AgarViewUtils.drawWorld(g, (toRender._1, toRender._2.values.toList), offsetX, offsetY)

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
