package de.exoticorn.modelconverter.fbx

import scala.collection.immutable._

sealed trait FbxValue

trait FbxNumber extends FbxValue {
  def toDouble: Double
}
case class FbxLong(value: Long) extends FbxNumber {
  def toDouble = value.toDouble
}
case class FbxDouble(value: Double) extends FbxNumber {
  def toDouble = value
}

case class FbxString(value: String) extends FbxValue

case class FbxIdentifier(value: String) extends FbxValue

case class FbxNode(tpe: String, attributes: FbxArray, children: Seq[FbxNode])

trait FbxArray {
  def toValueArray: FbxValueArray
}
case class FbxValueArray(array: Array[FbxValue]) extends FbxArray {
  def toValueArray = this
}
trait FbxNumberArray extends FbxArray {
  def doubleArray: Array[Double]
}
case class FbxLongArray(array: Array[Long]) extends FbxNumberArray {
  def toValueArray = FbxValueArray(array map (v => FbxLong(v)))
  def doubleArray = array map (_.toDouble)
}
case class FbxDoubleArray(array: Array[Double]) extends FbxNumberArray {
  def toValueArray = FbxValueArray(array map (v => FbxDouble(v)))
  def doubleArray = array
}