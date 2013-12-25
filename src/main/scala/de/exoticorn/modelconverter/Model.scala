package de.exoticorn.modelconverter

case class Material(texture: String)

case class MaterialMesh(material: Material, mesh: Mesh)