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
    var mesh = Mesh(
      Map(
        VertexAttributePosition -> vertices),
      indexList,
      polygons)

    val normalNode = model("LayerElementNormal")
    assert(normalNode("MappingInformationType").value == FbxString("ByPolygonVertex"))
    assert(normalNode("ReferenceInformationType").value == FbxString("Direct"))
    val normals = normalNode("Normals").attributes.asInstanceOf[FbxDoubleArray].array

    mesh = mesh.addPerPolygonVertexAttribute(VertexAttributeNormal, normals, 0.05)

    val uvNode = model("LayerElementUV")
    assert(uvNode("MappingInformationType").value == FbxString("ByPolygonVertex"))
    assert(uvNode("ReferenceInformationType").value == FbxString("IndexToDirect"))
    val uvs = uvNode("UV").attributes.asInstanceOf[FbxDoubleArray].array
    val uvIndices = uvNode("UVIndex").attributes.asInstanceOf[FbxLongArray].array

    mesh = mesh.addPerPolygonVertexIndexAttribute(VertexAttributeUV, uvs, uvIndices)

    mesh
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