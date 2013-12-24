package de.exoticorn.modelconverter

import scala.io.Source
import de.exoticorn.modelconverter.fbx._
import scala.collection.mutable.ArrayBuilder
import java.nio.{ ByteBuffer, ByteOrder }
import java.nio.file.Files
import java.io.File

object Main extends App {
  val root = Reader.load("tree.fbx")
  val mesh = Reader.toMesh(root, "Tree").toTriangles
  val positions = mesh.data(VertexAttributePosition)
  val normals = mesh.data(VertexAttributeNormal)
  val uvs = mesh.data(VertexAttributeUV)
  val numVertices = positions.size / 3
  val byteBuffer = ByteBuffer.allocate(8 + numVertices * 4 * (3 + 3 + 2) + mesh.indices.size * 2)
  byteBuffer.order(ByteOrder.LITTLE_ENDIAN)
  byteBuffer.putInt(numVertices)
  byteBuffer.putInt(mesh.indices.size)
  for (i <- 0 until numVertices) {
    byteBuffer.putFloat(positions(i * 3 + 0).toFloat)
    byteBuffer.putFloat(positions(i * 3 + 1).toFloat)
    byteBuffer.putFloat(positions(i * 3 + 2).toFloat)
    byteBuffer.putFloat(normals(i * 3 + 0).toFloat)
    byteBuffer.putFloat(normals(i * 3 + 1).toFloat)
    byteBuffer.putFloat(normals(i * 3 + 2).toFloat)
    byteBuffer.putFloat(uvs(i * 2 + 0).toFloat)
    byteBuffer.putFloat(1.0f - uvs(i * 2 + 1).toFloat)
  }
  for (i <- mesh.indices) {
    byteBuffer.putShort(i.toShort)
  }
  Files.write(new File("test.data").toPath(), byteBuffer.array())
}
