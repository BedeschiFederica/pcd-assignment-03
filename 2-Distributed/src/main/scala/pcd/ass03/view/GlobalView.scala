package pcd.ass03.view

import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import pcd.ass03.Message
import pcd.ass03.model.WorldManager.WorldMessage
import pcd.ass03.model.{World, WorldManager}

import java.awt.Graphics2D
import javax.swing.SwingUtilities
import scala.concurrent.duration.DurationInt
import scala.swing.*

object GlobalView:

  trait GlobalViewMessage extends Message
  object GlobalViewMessage:
    case class RenderWorld(world: World) extends GlobalViewMessage
    case class Flush() extends GlobalViewMessage

  import GlobalViewMessage.*
  export AgarViewUtils.*

  val Service: ServiceKey[GlobalViewMessage] = ServiceKey[GlobalViewMessage]("GlobalRenderService")
  def apply(width: Int, height: Int)(frameRate: Double = 60): Behavior[GlobalViewMessage | Receptionist.Listing] =
    Behaviors.setup: ctx =>
      ctx.system.receptionist ! Receptionist.Register(Service, ctx.self)
      val listingAdapter: ActorRef[Receptionist.Listing] = ctx.messageAdapter(listing => listing)
      ctx.system.receptionist ! Receptionist.Subscribe(WorldManager.Service, listingAdapter)
      GlobalViewImpl(width, height)(frameRate).receive

  private case class GlobalViewImpl(width: Int, height: Int)(frameRate: Double):
    private var worldActor: Option[ActorRef[WorldMessage]] = Option.empty
    private var world: Option[World] = Option.empty

    val receive: Behavior[GlobalViewMessage | Receptionist.Listing] = Behaviors.setup: ctx =>
      ctx.system.whenTerminated.onComplete(_ => frame.close())(using ctx.executionContext)
      Behaviors.withTimers: timers =>
        timers.startTimerAtFixedRate(Flush(), ((1 / frameRate) * 1000).toInt.milliseconds)
        Behaviors.receiveMessagePartial:
          case msg: Receptionist.Listing =>
            if msg.serviceInstances(WorldManager.Service).toList.nonEmpty then
              val service = msg.serviceInstances(WorldManager.Service).toList.head
              if !worldActor.contains(service) then worldActor = Some(service)
            Behaviors.same
          case RenderWorld(newWorld) =>
            world = Some(newWorld)
            update()
            Behaviors.same
          case Flush() =>
            update()
            Behaviors.same

    private val panel = new FlowPanel:
      override def paintComponent(g: Graphics2D): Unit =
        super.paintComponent(g)
        if world.nonEmpty then AgarViewUtils.drawWorld(g, world.get)

    private val frame = new MainFrame:
      title = "Agar.io - Global View"
      preferredSize = new Dimension(width, height)
      contents = panel
    frame.open()

    def update(): Unit = SwingUtilities.invokeLater(() => panel.repaint())
