package com.intel.cg.hpfd.madisonbay

/** Writable by hardware
  *
  * Applied to fields which can be generically written to by the hardware, allows writes of any value.
  */
trait HardwareWritable[P <: RdlRegister[P], A] { this: RdlField[P, A] =>
  def set(value: A): P = write(value)
  def modify(f: A => A): P = write(f(read()))
}
