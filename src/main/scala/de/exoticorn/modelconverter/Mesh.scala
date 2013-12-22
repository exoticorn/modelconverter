package de.exoticorn.modelconverter

import de.exoticorn.math._
import scala.collection.immutable._
import scala.collection.mutable.ArrayBuilder

sealed class VertexAttribute(val size: Int)
case object VertexAttributePosition extends VertexAttribute(3)
case object VertexAttributeNormal extends VertexAttribute(3)

sealed trait Polygons {
  def vertexIndex(polygonIndex: Int): Int
  def vertexCount(polygonIndex: Int): Int
  def size: Int
}
case class Triangles(val size: Int) extends Polygons {
  def vertexIndex(polygonIndex: Int) = polygonIndex * 3
  def vertexCount(polygonIndex: Int) = 3
}
case class MixedPolygons(offsets: Array[Int]) extends Polygons {
  def vertexIndex(polygonIndex: Int) = offsets(polygonIndex)
  def vertexCount(polygonIndex: Int) = offsets(polygonIndex + 1) - offsets(polygonIndex)
  def size = offsets.size - 1
}

case class Mesh(data: Map[VertexAttribute, Array[Double]], indices: Array[Int], polygons: Polygons) {
  assert(data.isDefinedAt(VertexAttributePosition))

  def toTriangles: Mesh = {
    val builder = ArrayBuilder.make[Int]
    for (polygonIndex <- 0 until polygons.size) {
      var base = polygons.vertexIndex(polygonIndex)
      for (i <- 1 to (polygons.vertexCount(polygonIndex) - 2)) {
        builder += indices(base)
        builder += indices(base + i)
        builder += indices(base + i + 1)
      }
    }
    val newIndices = builder.result()
    Mesh(data, newIndices, Triangles(newIndices.size / 3))
  }

  def addPerPolygonVertexAttribute(attr: VertexAttribute, aData: Array[Double], tolerance: Double): Mesh = {
    val reverseMapping = scala.collection.mutable.Map.empty[Int, Int]
    for ((i, j) <- indices.zipWithIndex) {
      reverseMapping += j -> i
    }
    val oldData = data map {
      case (a, d) =>
        val attrSize = a.size
        val newD = new Array[Double](reverseMapping.size * a.size)
        for (i <- 0 until reverseMapping.size) {
          val base = reverseMapping(i) * attrSize
          for (j <- 0 until attrSize) {
            newD(i * attrSize + j) = d(base + j)
          }
        }
        a -> newD
    }
    Mesh(oldData + (attr -> aData), (0 until indices.size).toArray, polygons)
  }
}