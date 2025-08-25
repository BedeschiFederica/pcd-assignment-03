package pcd.ass03.model

object EatingLogic:
  private val MassMargin = 1.1 // 10% bigger to eat

  def updateWorld(world: World): World =
    var newWorld = world
    world.players.map(_.id).foreach: id =>
      newWorld.playerById(id).collect {case player => newWorld = updateWorldAfterMovement(newWorld, player)}
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
    collides(player, other) && player.mass > other.mass * MassMargin

  // Check if two entities collide
  private def collides(e1: Entity, e2: Entity): Boolean = e1.distanceTo(e2) < (e1.radius + e2.radius)

/** Object responsible for AI movement logic, separate from the game state management */
object AIMovement:

  /** Finds the nearest food for a given player in the world
   *
   * @param player
   * the ID of the player for whom to find the nearest food
   * @param world
   * the current game world containing players and food
   * @return
   */
  private def nearestFood(player: String, world: World): Option[Food] =
    world.foods
      .sortBy(food => world.playerById(player).map(p => p.distanceTo(food)).getOrElse(Double.MaxValue))
      .headOption

  /** Moves the AI toward the nearest food. */
  def moveAI(name: String, world: World): (Double, Double) =
    val aiOpt = world.playerById(name)
    val foodOpt = nearestFood(name, world)
    (aiOpt, foodOpt) match
      case (Some(ai), Some(food)) =>
        val dx = food.pos.x - ai.pos.x
        val dy = food.pos.y - ai.pos.y
        val distance = math.hypot(dx, dy)
        if distance > 0 then
          (dx / distance, dy / distance)
        else
          (0, 0)
      case _ => (0, 0) // Do nothing if AI or food doesn't exist*/

object EndGameLogic:
  private val MassToWin = 1000

  def getGameWinner(players: Seq[Player]): Option[Player] = players.find(_.mass >= MassToWin)
