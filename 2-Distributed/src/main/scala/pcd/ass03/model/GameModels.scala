package pcd.ass03.model

case class Position(x: Double, y: Double)

sealed trait Entity:
  def id: String
  def mass: Double
  def pos: Position
  def radius: Double = math.sqrt(mass / math.Pi)
  def distanceTo(other: Entity): Double = math.hypot(pos.x - other.pos.x, pos.y - other.pos.y)

case class Player(id: String, pos: Position, mass: Double) extends Entity:
  def grow(entity: Entity): Player = copy(mass = mass + entity.mass)

case class Food(id: String, pos: Position, mass: Double = 100.0) extends Entity

case class World(width: Int, height: Int, players: Seq[Player], foods: Seq[Food]):
  def playersExcludingSelf(player: Player): Seq[Player] = players.filterNot(_.id == player.id)

  def playerById(id: String): Option[Player] = players.find(_.id == id)

  def updatePlayer(player: Player): World = copy(players = players.map(p => if (p.id == player.id) player else p))

  def removePlayers(ids: Seq[Player]): World = copy(players = players.filterNot(p => ids.map(_.id).contains(p.id)))

  def removeFoods(ids: Seq[Food]): World = copy(foods = foods.filterNot(f => ids.contains(f)))
