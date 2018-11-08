package com.intel.cg.hpfd.madisonbay.wm.utils.extensions

object UIntegers {

  val MaskLower16: Long         = 0x000000000000ffffL

  val MaskUpper16From32: Long   = 0x00000000ffff0000L

  val MaskLower8: Long          = 0x00000000000000ffL

  val MaskUpper8From16: Long    = 0x000000000000ff00L

  val MaskLower32: Long         = 0x00000000ffffffffL

  val MaskUpper32From64: Long   = 0xffffffff00000000L

  def toInt(v: Short): Int            = (v & MaskLower16).toInt

  def toInt(v: Byte): Int             = (v & MaskLower8).toInt

  def getLower8(v: Long): Int         = (v & MaskLower8).toInt

  def getLower16(v: Long): Int        = (v & MaskLower16).toInt

  def getUpper8From16(v: Long): Int   = (v & MaskUpper8From16).toInt

  def getUpper16From32(v: Long): Int  = (v & MaskUpper16From32).toInt

  def getLower32(v: Long): Long       = v & MaskLower32

  def getUpper32From64(v: Long): Long = v & MaskUpper32From64

}
