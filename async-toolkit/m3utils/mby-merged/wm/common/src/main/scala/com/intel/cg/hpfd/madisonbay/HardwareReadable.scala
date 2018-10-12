package com.intel.cg.hpfd.madisonbay

/** Readable by hardware
  *
  * Applied to RdlFields which can be read by hardware (no known examples of cases where this trait does not apply)
  */
trait HardwareReadable[A] { this: RdlField[_, A] =>
  def apply(): A = read()
  def get(): A = read()
}
