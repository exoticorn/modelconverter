package de.exoticorn.math

final case class Plane(normal: Vector3, t: Double) {
  def height(pos: Vector3) = normal * pos - t
}

object Plane {
  def apply(v1: Vector3, v2: Vector3, v3: Vector3) = {
    val normal = (v3 - v1).crossProduct(v2 - v1).normalize
    new Plane(normal, normal * v1)
  }
}

