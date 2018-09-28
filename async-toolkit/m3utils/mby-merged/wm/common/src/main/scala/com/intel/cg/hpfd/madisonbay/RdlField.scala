package com.intel.cg.hpfd.madisonbay

import com.intel.cg.hpfd.madisonbay.Encode.encode


/** Field of [[RdlRegister]].
  */
trait RdlField[P <: RdlRegister[P], E] {
  /** Parent register */
  val reg: P

  /** Alias for encoding */
  final type encode = E

  /** Encoding implicit parameter. */
  val en: Encode[E]

  /** Name as defined by RDL standard (as "name").
    *
    * Defaults to implementor's RDL name.
    */
  val name: String

  /** Description as defined by RDL standard (as "desc") */
  val desc = ""

  /** Integer range in bits, e.g. 0 to 8 means first 8 bits.
    *
    * Should be:
    * * literal (only accepted forms: {{{x to y}}} or {{{x until y}}})
    * * continuous (no {{{by}}})
    * * increasing (end > start)
    * * non-empty
    * * subrange of {{{ 0 until reg.accesswidth }}}
    * * mutually exclusive (non-overlapping) with reg's other fields' ranges
    */
  val range: Range

  /** Value to be set when resetting. */
  def resetValue: E = en.default

  /** Raw (bit) reset value. */
  final def rawResetValue: Long = en.toRaw(resetValue)

  /** Reset the field.
    * @return Register after the change
    * @note Not optimized for whole-register resets.
    */
  def reset(): P = write(resetValue)

  /** Read from field. */
  protected def read(): E = en.fromRaw(reg.read(range))

  /** Write to field.
    *
    * @return parent after the change */
  protected def write(value: E): P = reg.write(range, en.toRaw(value))

  /** Descriptive string.
    *
    * Could be useful for debug purposes.
    */
  override def toString: String = s"Field[${en.typeStr}] $name of $reg"
}
object RdlField {
  /** Example implementor */
  class Example(val reg: RdlRegister.Example) extends RdlField[RdlRegister.Example, Long] {
    val en = encode[Long]
    val name = "RdlFieldExample"
    val range = 0 until 64
  }
}
