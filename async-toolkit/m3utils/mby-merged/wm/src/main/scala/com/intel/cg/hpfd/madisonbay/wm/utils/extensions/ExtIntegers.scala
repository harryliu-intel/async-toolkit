package com.intel.cg.hpfd.madisonbay.wm.utils.extensions

import ExtIntegers._

object ExtIntegers {

  val SaturationUByte: Long   = 0xff

  val SaturationUShort: Long  = 0xffff

  val SaturationUInt: Long    = 0xffffffff

}

trait ExtIntegers[A] {

  def addWithSaturation(number1: A, number2: Long, limit: Long): A

  def incWithSaturation(number: A, limit: Long): A = addWithSaturation(number, 1, limit)

  def incWithUByteSaturation(number: A): A = incWithSaturation(number, SaturationUByte)

  def incWithUShortSaturation(number: A): A = incWithSaturation(number, SaturationUShort)

  def incWithUIntSaturation(number: A): A = incWithSaturation(number, SaturationUInt)

}
