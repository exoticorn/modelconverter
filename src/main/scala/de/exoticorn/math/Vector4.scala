package de.exoticorn.math

final case class Vector4(x: Double, y: Double, z: Double, w: Double) {
  def +(o: Vector4) = new Vector4(x + o.x, y + o.y, z + o.z, w + o.w)
  def -(o: Vector4) = new Vector4(x - o.x, y - o.y, z - o.z, w - o.w)
  def *(f: Double) = new Vector4(x * f, y * f, z * f, w * f)
  def *(o: Vector4) = x * o.x + y * o.y + z * o.z + w * o.w
  def /(f: Double) = this * (1 / f)

  def length = math.sqrt(x * x + y * y + z * z + w * w)
  def normalize = this / length
}

object Vector4 {
  val xAxis = new Vector4(1, 0, 0, 0)
  val yAxis = new Vector4(0, 1, 0, 0)
  val zAxis = new Vector4(0, 0, 1, 0)
  val wAxis = new Vector4(0, 0, 0, 1)
}

