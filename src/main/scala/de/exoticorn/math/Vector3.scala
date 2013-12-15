package de.exoticorn.math

final case class Vector3(x: Double, y: Double, z: Double) {
  def +(o: Vector3) = new Vector3(x + o.x, y + o.y, z + o.z)
  def -(o: Vector3) = new Vector3(x - o.x, y - o.y, z - o.z)
  def *(f: Double) = new Vector3(x * f, y * f, z * f)
  def *(o: Vector3) = x * o.x + y * o.y + z * o.z
  def /(f: Double) = this * (1 / f)

  def crossProduct(o: Vector3) =
    Vector3(
      y * o.z - z * o.y,
      z * o.x - x * o.z,
      x * o.y - y * o.x)

  def length = math.sqrt(x * x + y * y + z * z)
  def normalize = this / length
}

object Vector3 {
  val xAxis = new Vector3(1, 0, 0)
  val yAxis = new Vector3(0, 1, 0)
  val zAxis = new Vector3(0, 0, 1)
}

