package com.intel.cg.hpfd.madisonbay

import com.intel.cg.hpfd.madisonbay.Encode.encode

import scala.reflect.ClassTag


/** Field of [[RdlRegister]].
  */
trait RdlField[P <: RdlField[P, E], E] {
  /** Alias for encoding */
  final type encode = E

  /** Name as defined by RDL standard (as "name").
    *
    * Defaults to implementor's RDL name.
    */
  final def name: String = companion.name

  /** Description as defined by RDL standard (as "desc") */
  def desc: String = companion.desc

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

  /** Width of the field. */
  final def width: Int = range.last - range.start + 1

  /** Position in register */
  final def pos: Int = range.start

  /** Encoding implicit parameter. */
  final def en: Encode[E] = companion.en

  /** Value to be set when resetting. */
  val resetValue: E = en.default

  /** Raw (bit) reset value. */
  final def rawResetValue: BitVector = en.toRaw(resetValue)

  /** Reset the field.
    * @return Register after the change
    * @note Not optimized for whole-register resets.
    */
  final def reset(): P = write(resetValue)

  /** State part of the field.
    */
  val value: E

  /** Read from field. */
  protected def read(): E = value

  /** Raw copy used internally. */
  protected def rawCopy(value: E = value): P = companion.apply(value)

  /** Write to field. Returns a new one (Lens styel). */
  protected def write(value: E): P = rawCopy(value = value)

  /** Descriptive string.
    *
    * Could be useful for debug purposes.
    */
  override def toString: String = s"Field[${en.typeStr}] $name"

  /** Companion object. */
  def companion: RdlFieldCompanion[P, E]
}

abstract class RdlFieldCompanion[P <: RdlField[P, E]: ClassTag, E] { companion =>
  /** Instances' encoding. */
  def en: Encode[P#encode]

  /** Instances' reset value. */
  def resetValue: P#encode = en.default

  /** Instances' name. */
  def name: String

  /** Instances' description */
  def desc: String = ""

  /** Width of the field */
  def width: Int

  /** Constructor from value. */
  def apply(value: P#encode): P

  /** Default Constructor from value. */
  final def apply(): P = apply(resetValue)

  /** Default encoding for this register type. */
  implicit val encodeDefault: Encode[P] = new Encode[P] {
    def default: P = companion.apply(companion.resetValue)
    def toRaw(value: P): BitVector = value.en.toRaw(value.value)
    def fromRaw(bits: BitVector): P = companion.apply(en.fromRaw(bits))
    val size: Int = companion.width
  }
}
