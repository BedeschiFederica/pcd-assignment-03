package pcd.ass03

case class P2d(x: Double, y: Double):
  def +(v: V2d): P2d = P2d(x + v.x, y + v.y)

  def -(v: P2d): V2d = V2d(x - v.x, y - v.y)

  def distance(p: P2d): Double =
    val dx = p.x - x
    val dy = p.y - y
    Math.sqrt(dx * dx + dy * dy)

case class V2d(x: Double, y: Double):
  def +(v: V2d): V2d = V2d(x + v.x, y + v.y)

  def abs: Double = Math.sqrt(x * x + y * y)

  def getNormalized: V2d =
    val module = Math.sqrt(x * x + y * y)
    V2d(x / module, y / module)

  def *(fact: Double): V2d = V2d(x * fact, y * fact)