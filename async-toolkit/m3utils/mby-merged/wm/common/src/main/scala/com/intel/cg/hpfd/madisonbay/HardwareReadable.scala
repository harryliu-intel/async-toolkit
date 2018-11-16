package com.intel.cg.hpfd.madisonbay

/** Readable by hardware
  *
  * Applied to RdlFields which can be read by hardware (no known examples of cases where this trait does not apply)
  */
trait HardwareReadable[E] { this: RdlField[_, E] =>
  def apply(): E = read()
  def get(): E = read()
}
