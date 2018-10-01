//scalastyle:off
package com.intel.cg.hpfd.madisonbay

import com.intel.cg.hpfd.madisonbay.Memory._

//TODO: regwidth, accesswidth, alignment --- all are instance-specific, in fact

/** Register as defined in RDL.
  *
  * When using [[com.intel.cg.hpfd.csr.macros.annotations.reg]],
  * all of register's alignments should be expressed as literals of bits, bytes or plain integers.
  */
trait RdlRegister[P <: RdlRegister[P]] {
  /** Internal raw (bit) state shared by all fields. */
  val state: Long

  /** Range the register occupies.
    *
    * As given per Addressing.
    * Must be contained in parent's address range.
    */
  val range: AddressRange

  /** Starting address. */
  final def addr: Address = range.pos

  /** Starting address (alias for addr). */
  final def pos: Address = range.pos

  /** First free address after register. */
  final def lim: Address = range.lim

  /** Width as defined by RDL standard (as "regwidth")
    *
    * Has to be no smaller than [[accesswidth]].
    *
    * Should be no greater than 64 bits (machine word size).
    */
  val regwidth: Alignment = 8.bytes.toAlignment

  /** Access width as defined by RDL standard (as "accesswidth")
    *
    * Defaults to [[regwidth]].
    * For each if its fields, {{{ _.range }}} should fit in {{{ 0 until accesswidth }}}
    */
  val accesswidth: Alignment = regwidth

  /** Access width as defined by RDL standard (as "alignment")
    *
    * When using [[com.intel.cg.hpfd.csr.macros.annotations.reg]] it should be specified without {{{ Some }}}
    * ({{{ None }}} is equivalent of "not specified at declaration side")
    */
  val alignment: Option[Alignment] = None

  /** Raw, unsafe deep copy. */
  def rawCopy(range: AddressRange = range, state: Long = state): P

  /** Deep copy with address altered. **/
  final def copy(addr: Address): P = companion.apply(addr, state)

  /** Deep copy with address range and/or state changed.
    *
    * Overridable for performance.
    */
  def copy(range: AddressRange = range, state: Long = state): P = {
    if (range != this.range) {
      companion.apply(range.pos, state)
    }
    else {
      rawCopy(range, state)
    }
  }

  /** Copy with state altered. */
  def copy(newState: Long): P = rawCopy(range, newState)

  /** Deep copy **/
  final def copy(): P = rawCopy(range, state)

  /** Read raw value. */
  final def read: Long = state

  /** Write raw value. */
  final def write(value: Long): P = copy(value)

  /** Read raw value at certain range.
    *
    * @note Unsafe, doesn't check the range.
    */
  final def read(range: Range): Long = (state >> range.start) & ((1L << range.size) - 1L)

  /** Write raw value at certain range.
    *
    * @note Unsafe, doesn't check the range.
    */
  def write(range: Range, value: Long): P = write(RdlRegister.writeState(state, range, value))

  /** Efficient reset of all fields.
    * @return register after reset
    */
  def reset(): P = companion.apply(addr)

  /** Name.
    *
    * Defaults to implementor's RDL name.
    */
  val name: String

  /** Companion object. */
  val companion: RdlRegisterCompanion[P]

  override def toString: String = s"Register $name at $addr with value $state"
}
object RdlRegister {
  /** State after write at certain range.
    *
    * @note Unsafe, doesn't check the range.
    */
  def writeState(state: Long, range: Range, value: Long): Long = {
    val mask = ~(((1L << range.size) - 1L) << range.start)
    (state & mask) | (value << range.start)
  }

  /** Example implementor */
  class Example(val state: Long = 0xDEADBEEFl) extends RdlRegister[Example] {
    val range: AddressRange = AddressRange(Address(0.bits), 64.bits)
    val name = "RdlRegExample"
    val companion = Example
    def rawCopy(range: AddressRange = range, state: Long = state) = new Example(state)
  }
  object Example extends RdlRegisterCompanion[Example] {
    def apply(addr: Address) = new Example()
    def apply(addr: Address, state: Long) = new Example(state)
  }
}

trait RdlRegisterCompanion[P <: RdlRegister[P]] {
  /** Constructor from full address and state information. */
  def apply(addr: Address, state: Long): P

  /** Reset constructor. */
  def apply(addr: Address): P

  /** Default for [[RdlRegister]]'s regwidth. */
  val regwidth: Alignment = 8.bytes.toAlignment

  /** Default for [[RdlRegister]]'s accesswidth. */
  val accesswidth: Alignment = regwidth

  /** Default for [[RdlRegister]]'s alignment. */
  val alignment: Option[Alignment] = None
}
