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
    case pid if pid.startsWith("p") =>
      val idx = pid.drop(1).toIntOption.getOrElse(0)
      playerPalette(idx % playerPalette.length)
    case _ => Color.gray

  def drawWorld(g: Graphics2D, world: World, offsetX: Double = 0, offsetY: Double = 0): Unit =
    def toScreenCenter(pos: Position, radius: Int): (Int, Int) =
      ((pos.x - offsetX - radius).toInt, (pos.y - offsetY - radius).toInt)

    def toScreenLabel(pos: Position): (Int, Int) =
      ((pos.x - offsetX - playerLabelOffsetX).toInt, (pos.y - offsetY - playerLabelOffsetY).toInt)

    // Draw foods
    g.setColor(Color.green)
    world.foods.foreach: food =>
      val radius = food.radius.toInt
      val diameter = radius * 2
      val (foodX, foodY) = toScreenCenter(food.pos, radius)
      g.fillOval(foodX, foodY, diameter, diameter)

    // Draw players
    world.players.foreach: player =>
      val radius = player.radius.toInt
      val diameter = radius * 2
      val (borderX, borderY) = toScreenCenter(player.pos, radius)
      g.setColor(playerBorderColor)
      g.drawOval(borderX, borderY, diameter, diameter)
      g.setColor(playerColor(player.id))
      val (innerX, innerY) = toScreenCenter(player.pos, radius - playerInnerOffset)
      g.fillOval(innerX, innerY, diameter - playerInnerBorder, diameter - playerInnerBorder)
      g.setColor(playerBorderColor)
      val (labelX, labelY) = toScreenLabel(player.pos)
      g.drawString(player.id, labelX, labelY)
