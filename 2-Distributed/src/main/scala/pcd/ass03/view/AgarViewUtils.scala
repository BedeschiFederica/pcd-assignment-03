package pcd.ass03.view

import pcd.ass03.model.{Position, World}

import java.awt.{Color, Graphics2D}

object AgarViewUtils:

  private val playerBorderColor = Color.black
  private val playerLabelOffsetX = 10
  private val playerLabelOffsetY = 0
  private val playerInnerOffset = 2
  private val playerInnerBorder = 4
  private val playerPalette: Array[Color] =
    Array(Color.blue, Color.orange, Color.cyan, Color.pink, Color.yellow, Color.red, Color.green, Color.lightGray)

  private def playerColor(id: String): Color = id match
    case pid if pid.startsWith("Player") =>
      val idx = pid.drop(6).toIntOption.getOrElse(0)
      playerPalette(idx % playerPalette.length)
    case _ => Color.gray

  def drawWorld(
      g: Graphics2D,
      entities: (Map[String, (Position, Double)], List[(Position, Double)]),
      offsetX: Double = 0,
      offsetY: Double = 0
  ): Unit =
    def toScreenCenter(x: Double, y: Double, radius: Int): (Int, Int) =
      ((x - offsetX - radius).toInt, (y - offsetY - radius).toInt)

    def toScreenLabel(x: Double, y: Double): (Int, Int) =
      ((x - offsetX - playerLabelOffsetX).toInt, (y - offsetY - playerLabelOffsetY).toInt)

    // Draw foods
    g.setColor(Color.red)
    entities._2.foreach: food =>
      println(s"food pos: ${food._1}")
      val radius = food._2.toInt
      val diameter = radius * 2
      val (foodX, foodY) = toScreenCenter(food._1.x, food._1.y, radius)
      g.fillOval(foodX, foodY, diameter, diameter)

    // Draw players
    entities._1.foreach { case id -> (pos, radius) =>
      val diameter = radius.toInt * 2
      val (borderX, borderY) = toScreenCenter(pos.x, pos.y, radius.toInt)
      g.setColor(playerBorderColor)
      g.drawOval(borderX, borderY, diameter, diameter)
      g.setColor(playerColor(id))
      val (innerX, innerY) = toScreenCenter(pos.x, pos.y, radius.toInt - playerInnerOffset)
      g.fillOval(innerX, innerY, diameter - playerInnerBorder, diameter - playerInnerBorder)
      g.setColor(playerBorderColor)
      val (labelX, labelY) = toScreenLabel(pos.x, pos.y)
      g.drawString(id, labelX, labelY)
    }
