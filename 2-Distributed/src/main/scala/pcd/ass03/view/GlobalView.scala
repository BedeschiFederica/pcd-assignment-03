package pcd.ass03.view

import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior, PostStop}
import pcd.ass03.Message
import pcd.ass03.model.{World, WorldManager}
import WorldManager.WorldMessage

import java.awt.Graphics2D
import javax.swing.SwingUtilities
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.swing.*

object GlobalView:

  trait GlobalViewMessage extends Message
  object GlobalViewMessage:
    case class RenderWorld(world: World) extends GlobalViewMessage
    case class Flush() extends GlobalViewMessage

  import GlobalViewMessage.*
  import AgarViewUtils.*

  val Service: ServiceKey[GlobalViewMessage] = ServiceKey[GlobalViewMessage]("GlobalRenderService")
  def apply(width: Int, height: Int)(frameRate: FiniteDuration): Behavior[GlobalViewMessage | Receptionist.Listing] =
    Behaviors.setup: ctx =>
      ctx.system.receptionist ! Receptionist.Register(Service, ctx.self)
      val listingAdapter: ActorRef[Receptionist.Listing] = ctx.messageAdapter(listing => listing)
      ctx.system.receptionist ! Receptionist.Subscribe(WorldManager.Service, listingAdapter)
      GlobalViewImpl(width, height)(frameRate).receive

  private case class GlobalViewImpl(width: Int, height: Int)(frameRate: FiniteDuration):
    private var worldActor: Option[ActorRef[WorldMessage]] = Option.empty
    private var world: Option[World] = Option.empty

    val receive: Behavior[GlobalViewMessage | Receptionist.Listing] = Behaviors.setup: ctx =>
      ctx.system.whenTerminated.onComplete(_ => frame.close())(using ctx.executionContext)
      Behaviors.withTimers: timers =>
        timers.startTimerAtFixedRate(Flush(), frameRate)
        Behaviors.receiveMessagePartial[GlobalViewMessage | Receptionist.Listing]:
          case msg: Receptionist.Listing =>
            worldActor = Option(msg.serviceInstances(WorldManager.Service).toList).collect:
              case l if l.nonEmpty => l.head
            Behaviors.same
          case RenderWorld(newWorld) =>
            world = Some(newWorld)
            update()
            Behaviors.same
          case Flush() =>
            update()
            Behaviors.same
        .receiveSignal:
          case (_, PostStop) =>
            frame.close()
            Behaviors.stopped

    private val panel = new FlowPanel:
      override def paintComponent(g: Graphics2D): Unit =
        super.paintComponent(g)
        world.foreach(drawWorld(g, _))

    private val frame = new MainFrame:
      title = "Agar.io - Global View"
      preferredSize = new Dimension(width, height)
      contents = panel
    frame.open()

    def update(): Unit = SwingUtilities.invokeLater(() => panel.repaint())
