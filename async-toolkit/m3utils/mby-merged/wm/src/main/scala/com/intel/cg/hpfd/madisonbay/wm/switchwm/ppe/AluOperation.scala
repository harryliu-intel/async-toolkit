package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe

/**
  * ALU operation as used by parsing stages
  * @param rot rotation amount, up to 16 bits
  * @param mask mask field (12 bits)
  */
class AluOperation (val rot : Short, val mask : Short) {
  assert(rot < 16, "Rotate by 16 is the max allowed")
  assert((mask & 0xf000.toShort) == 0, "Only 12 bits of mask allowed")
  // no 'logical rotate' operator native to scala
  def apply(x : Short) : Short = (((x.toInt << 16 | x.toInt) >> rot.toInt) & mask).toShort
}
object AluOperation {
  // Build up from CSR encoding, High 4 bits are rotate, low 12 bits are mask
  def apply(x : Short) : AluOperation = {
    AluOperation(((x >> 12) & 0xF).toShort, (x & 0xFFF).toShort)
  }
  // Build up from CSR encoding, High 4 bits are rotate, low 12 bits are mask
  def apply(rot : Short, mask : Short) : AluOperation = {
    new AluOperation(rot.toShort, mask.toShort)
  }
}
