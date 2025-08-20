package pcd.ass03.view

import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import pcd.ass03.Message
import pcd.ass03.model.Position

import java.awt.Graphics2D
import javax.swing.SwingUtilities
import scala.swing.*
import scala.concurrent.duration.DurationInt


object PlayerView:
  trait PlayerViewMessage extends Message
  object PlayerViewMessage:
    case class Render(pos: Position, radius: Double, id: ActorRef[_]) extends PlayerViewMessage
    case class Flush() extends PlayerViewMessage

  import PlayerViewMessage.*

  val Service: ServiceKey[Render] = ServiceKey[Render]("RenderService")
  def apply(frameRate: Double = 60): Behavior[PlayerViewMessage] =
    Behaviors.setup: ctx =>
      val frontendGui = LocalView() // init the gui
      Behaviors.withTimers: timers =>
        timers.startTimerAtFixedRate(Flush(), ((1 / frameRate) * 1000).toInt.milliseconds)
        var toRender: Map[ActorRef[_], (Position, Double)] = Map.empty
        ctx.system.receptionist ! Receptionist.Register(Service, ctx.self)
        Behaviors.receiveMessage:
          case Render(pos, radius, id) =>
            ctx.log.info(s"render.. $id: $pos")
            toRender = toRender + (id -> (pos, radius))
            frontendGui.update(toRender.values.toList)
            Behaviors.same
          case Flush() =>
            frontendGui.update(toRender.values.toList)
            Behaviors.same

class LocalView extends MainFrame: //manager: MockGameStateManager,

  private var entities: List[(Position, Double)] = List.empty
  private val panel = new FlowPanel:
    listenTo(keys, mouse.moves)
    focusable = true
    visible = true
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

  title = s"Agar.io - Local View"
  preferredSize = new Dimension(400, 400)
  visible = true
  contents = panel

  def update(values: List[(Position, Double)]): Unit = SwingUtilities.invokeLater: () =>
      entities = values
      panel.repaint()

  /*def toScreenCenter(pos: Position, radius: Double): (Int, Int) =
    val (offsetX, offsetY) = (pos.x - preferredSize.width / 2.0, pos.y - preferredSize.height / 2.0)
    ((pos.x - offsetX - radius).toInt, (pos.y - offsetY - radius).toInt)

  def toScreenLabel(x: Double, y: Double): (Int, Int) =
    ((x - offsetX - playerLabelOffsetX).toInt, (y - offsetY - playerLabelOffsetY).toInt)*/

  /*reactions += { case e: event.MouseMoved =>
    val mousePos = e.point
    val playerOpt = manager.getWorld.players.find(_.id == playerId)
    playerOpt.foreach: player =>
      val dx = (mousePos.x - size.width / 2) * 0.01
      val dy = (mousePos.y - size.height / 2) * 0.01
      manager.movePlayerDirection(playerId, dx, dy)
    repaint()
  }*/
