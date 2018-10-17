//scalastyle:off regex.tuples
package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output

import FieldVector._

object FieldVector {

  val FieldVectorSize = 160

  //Table 4 - Fields and classification granularity
  //0..63 16b KEY16s (0..31)
  //64..95 8b KEY8s (0..31)
  //96..159 32b KEY32s (0..15)
  type Key16 = Short
  type Key8 = Byte
  type Key32 = Int

  implicit def nibblesKey16(n: (Int, Int, Int, Int)): Key16 = {
    (n._1 | n._2 << 4 | n._3 << 8 | n._4 << 12).toShort
  }

  def apply(array: Array[Byte]): FieldVector = new FieldVector(array)

  def apply(): FieldVector = apply(Array.ofDim[Byte](FieldVectorSize))

}

class FieldVector(array: Array[Byte]) {

  def updated(i: Int)(k: Key8): FieldVector = FieldVector(array.updated(i, k))

  def k16(i: Int): Key16 = {
    require(i %2 == 0)
    require(i < 64)

    array(i).toShort
  }

  def k32(i: Int): Key32 = {
    require(i %4 == 0)
    array(i).toInt
  }

  def k8(i: Int): Key32 = {
    require(i %4 == 0)
    array(i).toInt
  }

}
