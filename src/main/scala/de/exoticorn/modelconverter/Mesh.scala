package de.exoticorn.modelconverter

import de.exoticorn.math._
import scala.collection.immutable._

sealed class VertexAttribute(size: Int)
case object VertexAttributePosition extends VertexAttribute(3)
case object VertexAttributeNormal extends VertexAttribute(3)

case class Mesh(data: Map[VertexAttribute, Array[Double]], indices: Array[Int]) {
  assert(data.isDefinedAt(VertexAttributePosition))
}