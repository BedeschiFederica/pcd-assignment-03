package pcd.ass03

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors

import scala.swing.{FlowPanel, Graphics2D}
import java.awt.Color

trait DrawMessage
final case class DrawBoids() extends DrawMessage

object DrawerActor:
  private var boids: List[ActorRef[BoidMessage]] = List.empty
  private var boidsPosition: List[P2d] = List.empty
  private var _frameRate: Int = 0
  private var _panel = FlowPanel()
  private var counter = 0

  def apply(manager: Option[ActorRef[ManagerMessage]], boidsActor: List[ActorRef[BoidMessage]], width: Int,
            height: Int): Behavior[DrawMessage] =
    boids = boidsActor
    _panel = createPanel(width, height)
    drawing(manager.get)

  private val drawing: ActorRef[ManagerMessage] => Behavior[DrawMessage] = manager =>
    Behaviors.receivePartial:
      (context, message) => message match
        case DrawBoids() =>
          boidsPosition = List.empty
          boids.foreach(_ ! Ask(context.self))
          waitingPositions(manager)

  private val waitingPositions: ActorRef[ManagerMessage] => Behavior[DrawMessage] = manager =>
    Behaviors.receiveMessagePartial:
      case Send(pos, _, _) =>
        counter += 1
        boidsPosition +:= pos
        if counter == boids.size then
          counter = 0
          //frameRate = ???
          _panel.repaint()
          manager ! UpdatedView()
          drawing(manager)
        else
          Behaviors.same

  def panel(): FlowPanel = _panel

  def frameRate: Int = _frameRate
  def frameRate_=(frameRate: Int): Unit = _frameRate = frameRate

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
        g.drawString("Framerate: " + _frameRate, 10, 40)
