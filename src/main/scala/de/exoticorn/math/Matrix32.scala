package de.exoticorn.math

final case class Matrix32(rot: Matrix22, pos: Vector2) {
  def *(o: Vector2): Vector2 = rot * o + pos
  def *(o: Matrix32): Matrix32 = new Matrix32(rot * o.rot, this * o.pos)

  def inverse: Matrix32 = {
    val invRot = rot.inverse
    new Matrix32(invRot, invRot * pos * -1)
  }
}

object Matrix32 {
  def apply(x: Vector2, y: Vector2, pos: Vector2) = new Matrix32(new Matrix22(x, y), pos)
  val unit = new Matrix32(Matrix22.unit, Vector2(0, 0))
}

