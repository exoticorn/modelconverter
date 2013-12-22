package de.exoticorn.modelconverter

import scala.io.Source
import de.exoticorn.modelconverter.fbx._
import scala.collection.mutable.ArrayBuilder
import java.nio.{ ByteBuffer, ByteOrder }
import java.nio.file.Files
import java.io.File

object Main extends App {
  val root = Reader.load("test.fbx")
  val mesh = Reader.toMesh(root, "Suzanne").toTriangles
  val positions = mesh.data(VertexAttributePosition)
  val normals = mesh.data(VertexAttributeNormal)
  println((positions.size, normals.size))
  assert(positions.size == normals.size)
  val byteBuffer = ByteBuffer.allocate(8 + positions.size * 4 * 2 + mesh.indices.size * 2)
  byteBuffer.order(ByteOrder.LITTLE_ENDIAN)
  byteBuffer.putInt(positions.size)
  byteBuffer.putInt(mesh.indices.size)
  for (i <- 0 until (positions.size / 3)) {
    byteBuffer.putFloat(positions(i * 3 + 0).toFloat)
    byteBuffer.putFloat(positions(i * 3 + 1).toFloat)
    byteBuffer.putFloat(positions(i * 3 + 2).toFloat)
    byteBuffer.putFloat(normals(i * 3 + 0).toFloat)
    byteBuffer.putFloat(normals(i * 3 + 1).toFloat)
    byteBuffer.putFloat(normals(i * 3 + 2).toFloat)
  }
  for (i <- mesh.indices) {
    byteBuffer.putShort(i.toShort)
  }
  Files.write(new File("test.data").toPath(), byteBuffer.array())
}
