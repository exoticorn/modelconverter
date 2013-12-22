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
    val (polygons, indexList) = toPolygons(model("PolygonVertexIndex").attributes.asInstanceOf[FbxLongArray].array)
    val baseMesh = Mesh(
      Map(
        VertexAttributePosition -> vertices),
      indexList,
      polygons)

    val normalNode = model("LayerElementNormal")
    assert(normalNode("MappingInformationType").value == FbxString("ByPolygonVertex"))
    assert(normalNode("ReferenceInformationType").value == FbxString("Direct"))
    val normals = normalNode("Normals").attributes.asInstanceOf[FbxDoubleArray].array

    baseMesh.addPerPolygonVertexAttribute(VertexAttributeNormal, normals, 0.05)
  }

  def toPolygons(in: Array[Long]): (Polygons, Array[Int]) = {
    val indicesBuilder = ArrayBuilder.make[Int]
    val offsetsBuilder = ArrayBuilder.make[Int]
    offsetsBuilder += 0
    var index = 0
    while (index < in.length) {
      val i = in(index)
      indicesBuilder += (if (i < 0) -i - 1 else i).toInt
      if (i < 0) {
        offsetsBuilder += index + 1
      }
      index += 1
    }
    (MixedPolygons(offsetsBuilder.result()), indicesBuilder.result())
  }
}