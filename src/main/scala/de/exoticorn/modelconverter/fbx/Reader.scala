package de.exoticorn.modelconverter
package fbx

import scala.collection.immutable._
import java.io.File
import scala.io.Source
import scala.collection.mutable.ArrayBuilder

object Reader {
  def load(filename: String): FbxNode = load(new File(filename))

  def load(file: File): FbxNode = {
    val source = Source.fromFile(file)
    val lexer = new Lexer(source)
    Parser.parse(lexer)
  }

  def toMesh(root: FbxNode, name: String): Mesh = {
    val objects = root("Objects")
    val model = (objects find (_.attributes(0).asString == s"Model::$name")).get
    val vertices = model("Vertices").attributes.asInstanceOf[FbxDoubleArray].array
    val indexList = triangulateFbx(model("PolygonVertexIndex").attributes.asInstanceOf[FbxLongArray].array)
    Mesh(Map(VertexAttributePosition -> vertices), indexList)
  }

  def triangulateFbx(in: Array[Long]): Array[Int] = {
    val builder = ArrayBuilder.make[Int]
    var index = 0
    while (index < in.length) {
      var a = in(index)
      def inner(index: Int): Int = {
        val b = in(index)
        val c = in(index + 1)
        builder += a.toInt
        builder += b.toInt
        builder += (if (c >= 0) c else -c - 1).toInt
        if (c < 0) index + 2
        else inner(index + 1)
      }
      index = inner(index + 1)
    }
    builder.result()
  }
}