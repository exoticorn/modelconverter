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

  case class Objects(
    models: Map[String, FbxNode],
    textures: Map[String, FbxNode])

  def readObjects(root: FbxNode): Objects = {
    var models = Map.empty[String, FbxNode]
    var textures = Map.empty[String, FbxNode]
    for (obj <- root("Objects").children) {
      obj match {
        case FbxNode("Model", FbxString(name), _) => models += name -> obj
        case FbxNode("Texture", FbxString(name), _) => textures += name -> obj
        case _ =>
      }
    }
    Objects(models, textures)
  }

  def toMesh(root: FbxNode, name: String): Mesh = {
    val model = readObjects(root).models(name)
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

  def materialForMesh(root: FbxNode, name: String): Material = {
    val connections = root("Connections")
    val textures = readObjects(root).textures
    val textureConnections = connections.children map {
      _.attributes.toValueArray.array match {
        case Array(FbxString("OO"), FbxString(textureName), FbxString(`name`)) if textures.isDefinedAt(textureName) =>
          Some(textures(textureName))
        case _ => None
      }
    } collect { case Some(t) => t }
    val filename = textureConnections.head("FileName").value.asString
    Material(filename)
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