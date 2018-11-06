
package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.internal

import com.intel.cg.hpfd.madisonbay.wm.utils.extensions.UIntegers._

/**
  * Arithmetic Logic Unit
  *
  * ALU operation as used by parsing stages
 *
  * @param rotationAmount rotation amount, up to 16 bits
  * @param mask mask field (12 bits)
  */
class AluOperation(rotationAmount: Short, mask: Short) {

  assert(rotationAmount < 16, "Rotate by 16 is the max allowed")

  assert((mask & 0xf000.toShort) == 0, "Only 12 bits of mask allowed")

  // no 'logical rotate' operator native to scala
  def calculate(x: Short): Short =
    (
      ((getUpper16From32(x.toInt << 16) | getLower16(x.toInt)) >> rotationAmount.toInt) & getLower16(mask)
    ).toShort

  override def toString: String = s"AluOperation(rotation=$rotationAmount, mask=$mask)"
}

object AluOperation {

  // Build up from CSR encoding, High 4 bits are rotate, low 12 bits are mask
  def apply(rotationAndMask: Short): AluOperation = AluOperation(extractRotation(rotationAndMask), extractMask(rotationAndMask))

  def apply(rotationAmount: Short, mask: Short): AluOperation = new AluOperation(rotationAmount.toShort, mask.toShort)

  def extractRotation(rotationAndMask: Short): Short = ((getLower16(rotationAndMask) >> 12) & 0xF).toShort

  def extractMask(rotationAndMask: Short): Short = (rotationAndMask & 0xFFF).toShort
}
