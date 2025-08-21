package pcd.ass03.model

/*trait GameStateManager:
  def world: World
  def movePlayerDirection(id: String, position: Position): Unit

class MockGameStateManager(override val world: World, speed: Double = 10.0) extends GameStateManager:

  private var directions: Map[String, Position] = Map.empty

  // Move a player in a given direction (dx, dy)
  def movePlayerDirection(id: String, position: Position): Unit =
    directions = directions.updated(id, position)

  def tick(): Unit =
    directions.foreach:
      case (id, pos) =>
        world.playerById(id) match
          case Some(player) =>
            //world = updateWorldAfterMovement(updatePlayerPosition(player, dx, dy))
          case None =>
          // Player not found, ignore movement

  private def updatePlayerPosition(player: Player, dx: Double, dy: Double): Player =
    val newX = (player.pos.x + dx * speed).max(0).min(world.width)
    val newY = (player.pos.y + dy * speed).max(0).min(world.height)
    player.copy(player.id, pos = Position(newX, newY), player.mass)

  private def updateWorldAfterMovement(player: Player): World =
    val foodEaten = world.foods.filter(food => EatingManager.canEatFood(player, food))
    val playerEatsFood = foodEaten.foldLeft(player)((p, food) => p.grow(food))
    val playersEaten = world
      .playersExcludingSelf(player)
      .filter(player => EatingManager.canEatPlayer(playerEatsFood, player))
    val playerEatPlayers = playersEaten.foldLeft(playerEatsFood)((p, other) => p.grow(other))
    world
      .updatePlayer(playerEatPlayers)
      .removePlayers(playersEaten)
      .removeFoods(foodEaten)
*/