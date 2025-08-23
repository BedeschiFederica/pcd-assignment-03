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
    player.copy(player.id, pos = Position(newX, newY), player.mass)*/

object EatingLogic:
  private val MASS_MARGIN = 1.1 // 10% bigger to eat

  def updateWorld(world: World): World =
    var newWorld = world
    world.players.map(_.id).foreach: id =>
      newWorld.playerById(id) match
        case Some(player) => newWorld = updateWorldAfterMovement(newWorld, player)
        case None => // Player not found, ignore movement
    newWorld

  private def updateWorldAfterMovement(world: World, player: Player): World =
    val foodEaten = world.foods.filter(food => canEatFood(player, food))
    val playerEatsFood = foodEaten.foldLeft(player)((p, food) => p.grow(food))
    val playersEaten = world
      .playersExcludingSelf(player)
      .filter(player => canEatPlayer(playerEatsFood, player))
    val playerEatPlayers = playersEaten.foldLeft(playerEatsFood)((p, other) => p.grow(other))
    world
      .updatePlayer(playerEatPlayers)
      .removePlayers(playersEaten)
      .removeFoods(foodEaten)

  // Determines if a player can eat a food
  private def canEatFood(player: Player, food: Food): Boolean = collides(player, food) && player.mass > food.mass

  // Determines if a player can eat another player
  private def canEatPlayer(player: Player, other: Player): Boolean =
    collides(player, other) && player.mass > other.mass * MASS_MARGIN

  // Check if two entities collide
  private def collides(e1: Entity, e2: Entity): Boolean = e1.distanceTo(e2) < (e1.radius + e2.radius)