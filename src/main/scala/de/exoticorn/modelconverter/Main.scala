package de.exoticorn.modelconverter

import scala.io.Source
import de.exoticorn.modelconverter.fbx._
import scala.collection.mutable.ArrayBuilder
import java.nio.{ ByteBuffer, ByteOrder }
import java.nio.file.Files
import java.io.File

object Main extends App {
  val root = Reader.load("test.fbx")
  val mesh = Reader.toMesh(root, "Suzanne")
  val positions = mesh.data(VertexAttributePosition)
  val byteBuffer = ByteBuffer.allocate(8 + positions.size * 4 + mesh.indices.size * 2)
  byteBuffer.order(ByteOrder.LITTLE_ENDIAN)
  byteBuffer.putInt(positions.size)
  byteBuffer.putInt(mesh.indices.size)
  for (v <- positions) {
    byteBuffer.putFloat(v.toFloat)
  }
  for (i <- mesh.indices) {
    byteBuffer.putShort(i.toShort)
  }
  Files.write(new File("test.data").toPath(), byteBuffer.array())
}
