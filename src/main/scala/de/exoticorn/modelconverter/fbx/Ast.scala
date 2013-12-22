package de.exoticorn.modelconverter.fbx

import scala.collection.immutable._

case class FbxTypeError(str: String) extends Exception(str)

sealed trait FbxValue {
  def asLong: Long = throw new FbxTypeError(s"Tried to read $this as long")
  def asDouble: Double = throw new FbxTypeError(s"Tried to read $this as double")
  def asString: String = throw new FbxTypeError(s"Tried to read $this as string")
}

trait FbxNumber extends FbxValue
case class FbxLong(value: Long) extends FbxNumber {
  override def asDouble = value.toDouble
}
case class FbxDouble(value: Double) extends FbxNumber {
  override def asDouble = value
}

case class FbxString(value: String) extends FbxValue {
  override def asString = value
}

case class FbxIdentifier(value: String) extends FbxValue {
  override def asString = value
}

case class FbxNode(tpe: String, attributes: FbxArray, children: Seq[FbxNode]) {
  def apply(key: String): FbxNode = get(key).get
  def get(key: String): Option[FbxNode] = children find (_.tpe == key)
  def find(p: FbxNode => Boolean): Option[FbxNode] = children find p
  def value: FbxValue = {
    assert(attributes.size == 1)
    attributes(0)
  }
}

trait FbxArray {
  def toValueArray: FbxValueArray
  def size: Int
  def apply(index: Int): FbxValue
}
case class FbxValueArray(array: Array[FbxValue]) extends FbxArray {
  def toValueArray = this
  def size = array.size
  def apply(index: Int) = array(index)
}
trait FbxNumberArray extends FbxArray {
  def doubleArray: Array[Double]
}
case class FbxLongArray(array: Array[Long]) extends FbxNumberArray {
  def toValueArray = FbxValueArray(array map (v => FbxLong(v)))
  def doubleArray = array map (_.toDouble)
  def size = array.size
  def apply(index: Int) = FbxLong(array(index))
}
case class FbxDoubleArray(array: Array[Double]) extends FbxNumberArray {
  def toValueArray = FbxValueArray(array map (v => FbxDouble(v)))
  def doubleArray = array
  def size = array.size
  def apply(index: Int) = FbxDouble(array(index))
}