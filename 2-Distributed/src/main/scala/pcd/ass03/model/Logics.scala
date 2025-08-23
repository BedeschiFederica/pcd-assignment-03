package pcd.ass03.model

object EatingLogic:
  private val MassMargin = 1.1 // 10% bigger to eat

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
    collides(player, other) && player.mass > other.mass * MassMargin

  // Check if two entities collide
  private def collides(e1: Entity, e2: Entity): Boolean = e1.distanceTo(e2) < (e1.radius + e2.radius)

object EndGameLogic:
  private val MassToWin = 1000

  def getGameWinner(players: Seq[Player]): Option[Player] = players.find(_.mass >= MassToWin)
