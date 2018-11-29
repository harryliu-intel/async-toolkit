//scalastyle:off
package com.intel.cg.hpfd.madisonbay

import com.intel.cg.hpfd.madisonbay.Memory._

import scala.collection.immutable.HashMap
import scala.reflect.ClassTag
import monocle.{Lens, Optional}


/** Register as defined in RDL.
  *
  * When using [[com.intel.cg.hpfd.csr.macros.annotations.reg]],
  * all of register's alignments should be expressed as literals of bits, bytes or plain integers.
  */
trait RdlRegister[P <: RdlRegister[P]] {
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
  final def regwidth: Alignment = companion.regwidth

  /** Access width as defined by RDL standard (as "accesswidth")
    *
    * Defaults to [[regwidth]].
    * For each if its fields, {{{ _.range }}} should fit in {{{ 0 until accesswidth }}}
    */
  final def accesswidth: Alignment = companion.regwidth

  /** Access width as defined by RDL standard (as "alignment")
    *
    * When using [[com.intel.cg.hpfd.csr.macros.annotations.reg]] it should be specified without {{{ Some }}}
    * ({{{ None }}} is equivalent of "not specified at declaration side")
    */
  final def alignment: Option[Alignment] = companion.alignment

  /** Efficient reset of all fields.
    * @return register after reset
    */
  def reset(): P = companion.apply(addr)

  /** Name.
    *
    * Defaults to implementor's RDL name.
    */
  final def name: String = companion.name

  /** Description as defined by RDL standard (as "desc") */
  final def desc: String = companion.desc

  /** Companion object. */
  val companion: RdlRegisterCompanion[P]

  /** Type-erasured list of all fields in this register.
    *
    * Useful for best-try operations on fields, as well as serialization/deserialization.
    */
  def fields: List[RdlField[_, _]]

  /** How to serialize given register.
    *
    * Typeful equivalent of:
    * {{{
    *   fields.foldRight(BitVector.ofLength(regwidth))((f,v) => v | (f, f.start))
    * }}}
    */
  def serialize(): BitVector

  /** How to deserialize given register.
    *
    * Shortcut to calling on companion
    */
  final def deserialize(bits: BitVector): P = companion.deserialize(bits, range)

  override def toString: String = s"Register $name at $addr"
}

abstract class RdlRegisterCompanion[P <: RdlRegister[P] : ClassTag] { companion =>
  /** Reset constructor. */
  def apply(addr: Address): P

  /** Instances' name */
  val name: String

  /** Instances' description */
  val desc: String = ""

  /** Instances' regwidth. */
  val regwidth: Alignment = 8.bytes.toAlignment

  /** Instances' accesswidth. */
  val accesswidth: Alignment = regwidth

  /** Instances' alignment. */
  val alignment: Option[Alignment] = None

  /** Encoding for this register type. */
  implicit val encodeDefault: Encode[P] = encode(companion.regwidth.toLong.toInt)

  /** Encode when width is overrode (e.g. by Addressing). */
  def encode(width: Int): Encode[P] = new Encode[P] {
    override val size = width
    def default: P = companion.apply(Address(0.bits))  // arbitrary
    def toRaw(value: P): BitVector = value.serialize()
    def fromRaw(bits: BitVector): P = companion.deserialize(bits)
  }

  /** How to deserialize given register.
    *
    * The semantic is close to the following:
    * {{{
    *   val field1 = bits.extract[field1]
    *   val field2 = bits.extract[field2]
    *   ...
    *   apply(field1, field2, ...)
    * }}}
    *
    * Can't be done generically due to unknown field types.
    */
  def deserialize(bits: BitVector, range: AddressRange): P

  /** Deserialization to some default address. */
  def deserialize(bits: BitVector): P = deserialize(bits, AddressRange(Address(0), regwidth.toBits))  // arbitrary

  /** Whole-register bit state lens */
  final val _state: Lens[P, BitVector] =
    Lens[P, BitVector] {
      reg => reg.serialize
    } {
      newValue => _.deserialize(newValue)
    }

  /** Generates lenses from path to register's state. */
  def genOpticsLookup[A](me: P,
                         path: Optional[A, P]): HashMap[Address, Optional[A, BitVector]] = {
    HashMap(me.range.pos -> (path composeLens _state))
  }
}
