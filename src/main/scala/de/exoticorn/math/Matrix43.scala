package de.exoticorn.math

final case class Matrix43(rot: Matrix33, pos: Vector3) {
  def *(o: Vector3): Vector3 = rot * o + pos
  def *(o: Matrix43): Matrix43 = new Matrix43(rot * o.rot, rot * o.pos + pos)
  def inverse = {
    val inv = rot.inverse
    new Matrix43(inv, inv * pos * -1)
  }
}

object Matrix43 {
  def apply(x: Vector3, y: Vector3, z: Vector3, pos: Vector3) = new Matrix43(new Matrix33(x, y, z), pos)
  val unit = new Matrix43(Matrix33.unit, Vector3(0, 0, 0))
}

