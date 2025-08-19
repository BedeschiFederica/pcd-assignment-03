package pcd.ass03.view

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior, DispatcherSelector}
import pcd.ass03.model.*
import pcd.ass03.Configuration

import java.awt.Color
import scala.concurrent.{ExecutionContext, Future}
import scala.swing.{FlowPanel, Graphics2D}
import scala.util.Success

trait DrawMessage
final case class DrawBoids() extends DrawMessage
final case class UpdatedFrameRate(rate: Int) extends DrawMessage

object DrawerActor:
  private val FrameRate = 200

  private var boids: List[ActorRef[BoidMessage]] = List.empty
  private var boidsPosition: List[P2d] = List.empty
  private var frameRate: Int = 0
  private var _panel = FlowPanel()
  private var counter = 0
  private var t0: Long = 0

  def apply(manager: Option[ActorRef[ManagerMessage]], boidsActor: List[ActorRef[BoidMessage]], width: Int,
            height: Int): Behavior[DrawMessage] =
    boids = boidsActor
    _panel = createPanel(width, height)
    drawing(manager.get)

  def panel(): FlowPanel = _panel

  private val drawing: ActorRef[ManagerMessage] => Behavior[DrawMessage] = manager =>
    Behaviors.receive:
      (context, message) => message match
        case DrawBoids() =>
          boidsPosition = List.empty
          boids.foreach(_ ! Ask(context.self))
          waitingPositions(manager)
        case message => handleCommonMessages(drawing(manager))(message)

  private val waitingPositions: ActorRef[ManagerMessage] => Behavior[DrawMessage] = manager =>
    Behaviors.receive:
      (context, message) =>
        message match
          case Send(pos, _, _) =>
            counter += 1
            boidsPosition +:= pos
            if counter == boids.size then
              counter = 0
              updateFrameRate(context)
              _panel.repaint()
              manager ! UpdatedView()
              drawing(manager)
            else
              Behaviors.same
          case message => handleCommonMessages(waitingPositions(manager))(message)

  private def handleCommonMessages(oldState: Behavior[DrawMessage]): DrawMessage => Behavior[DrawMessage] =
    case UpdatedFrameRate(rate) =>
      frameRate = rate
      oldState
    case _ => Behaviors.unhandled

  private def updateFrameRate(ctx: ActorContext[DrawMessage]): Unit =
    val t1 = System.currentTimeMillis
    val dtElapsed = t1 - t0
    val frameRatePeriod = 1000 / FrameRate
    if dtElapsed < frameRatePeriod then
      frameRate = FrameRate
      given ExecutionContext =
        ctx.system.dispatchers.lookup(DispatcherSelector.fromConfig("my-blocking-dispatcher"))
      val f: Future[Unit] = Future(Thread.sleep(frameRatePeriod - dtElapsed))
      ctx.pipeToSelf(f):
        case Success(_) => UpdatedFrameRate(frameRate)
        case _ => throw IllegalStateException("Future unsuccessful.")
    else
      frameRate = (1000 / dtElapsed).toInt
      ctx.self ! UpdatedFrameRate(frameRate)
    t0 = System.currentTimeMillis

  private def createPanel(width: Int, height: Int): FlowPanel =
    new FlowPanel():
      override def paintComponent(g: Graphics2D): Unit =
        super.paintComponent(g)
        background = Color.WHITE
        val xScale: Double = width / Configuration.EnvironmentWidth
        g.setColor(Color.BLUE)
        boidsPosition.foreach: pos =>
          val px: Int = (width / 2 + pos.x * xScale).toInt
          val py: Int = (height / 2 - pos.y * xScale).toInt
          g.fillOval(px, py, 5, 5)
        g.setColor(Color.BLACK)
        g.drawString("Num. Boids: " + boids.size, 10, 25)
        g.drawString("Framerate: " + frameRate, 10, 40)
