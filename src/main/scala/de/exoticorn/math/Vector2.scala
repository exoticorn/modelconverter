package de.exoticorn.math

final case class Vector2(val x: Double, val y: Double) {
  def +(o: Vector2) = new Vector2(x + o.x, y + o.y)
  def -(o: Vector2) = new Vector2(x - o.x, y - o.y)
  def *(f: Double) = new Vector2(x * f, y * f)
  def *(o: Vector2) = x * o.x + y * o.y
  def /(f: Double) = this * (1 / f)

  def length = math.sqrt(x * x + y * y)
  def normalize = this / length

  def angle = math.atan2(y, x)
}

object Vector2 {
  def fromAngle(angle: Double, length: Double = 1) = new Vector2(math.cos(angle) * length, math.sin(angle) * length)
  val xAxis = new Vector2(1, 0)
  val yAxis = new Vector2(0, 1)
}

