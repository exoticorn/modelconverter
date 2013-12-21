package de.exoticorn.modelconverter

import scala.io.Source
import de.exoticorn.modelconverter.fbx._
import scala.collection.mutable.ArrayBuilder
import java.nio.{ ByteBuffer, ByteOrder }
import java.nio.file.Files
import java.io.File

object Main extends App {
  val source = Source.fromFile("test.fbx")
  val lexer = new Lexer(source)
  val root = Parser.parse(lexer)
  val objects = root("Objects")
  val model = (objects find (_.attributes(0).asString == "Model::Suzanne")).get
  val vertices = model("Vertices").attributes.asInstanceOf[FbxDoubleArray].array
  def triangulateFbx(in: Array[Long]): Array[Long] = {
    val builder = ArrayBuilder.make[Long]
    var index = 0
    while (index < in.length) {
      var a = in(index)
      def inner(index: Int): Int = {
        val b = in(index)
        val c = in(index + 1)
        builder += a
        builder += b
        builder += (if(c >= 0) c else -c - 1)
        if (c < 0) index + 2
        else inner(index + 1)
      }
      index = inner(index + 1)
    }
    builder.result()
  }
  val indexList = triangulateFbx(model("PolygonVertexIndex").attributes.asInstanceOf[FbxLongArray].array)
  val byteBuffer = ByteBuffer.allocate(8 + vertices.size * 4 + indexList.size * 2)
  byteBuffer.order(ByteOrder.LITTLE_ENDIAN)
  byteBuffer.putInt(vertices.size)
  byteBuffer.putInt(indexList.size)
  for (v <- vertices) {
    byteBuffer.putFloat(v.toFloat)
  }
  for (i <- indexList) {
    byteBuffer.putShort(i.toShort)
  }
  Files.write(new File("test.data").toPath(), byteBuffer.array())
}
