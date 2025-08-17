package pcd.ass03

import akka.actor.typed.ActorRef

import java.awt.Color
import scala.swing.*

class BoidsPanel(width: Int, height: Int) extends FlowPanel: // private val view: BoidsView
  private var _frameRate: Int = 0

  def frameRate: Int = _frameRate
  def frameRate_=(frameRate: Int): Unit = _frameRate = frameRate

  override protected def paintComponent(g: Graphics2D): Unit =
    super.paintComponent(g)
    background = Color.WHITE
    val xScale: Double = width / Configuration.EnvironmentWidth
    val boids: List[ActorRef[BoidMessage]] = List.empty //this.controller.getBoids
    g.setColor(Color.BLUE)
    boids.foreach: boid =>
      val x: Double = 100 //boid.getPos.x
      val y: Double = 100 //boid.getPos.y
      val px: Int = (width / 2 + x * xScale).toInt
      val py: Int = (height / 2 - y * xScale).toInt
      g.fillOval(px, py, 5, 5)
    g.setColor(Color.BLACK)
    g.drawString("Num. Boids: " + boids.size, 10, 25)
    g.drawString("Framerate: " + _frameRate, 10, 40)

