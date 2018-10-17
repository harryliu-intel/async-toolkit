//scalastyle:off
package com.intel.cg.hpfd.madisonbay

import scala.annotation.tailrec
import scala.reflect.api.Universe
import scala.reflect.runtime.{universe => runtimeUniverse}
import scala.reflect.macros.blackbox
import scala.language.implicitConversions

object Memory {
  //TODO: provide generalization over word size later on

  /** Any units of raw memory (bits, bytes etc). Should be a power of two (2 pow N) in bits.
    * @note All methods return in bits.
    * */
  sealed trait MemoryUnit extends Any with Ordered[MemoryUnit] {
    /** Main numerical value, as expressed by "N units", e.g. for 8.bytes it returns 8 */
    def toLong: Long

    /** Bit value */
    def toBits: Bits

    /** Byte value if full bytes, None otherwise. */
    def tryBytes: Option[Bytes]

    /** Floor of full byte value, e.g. 15.bits.fullBytes == 1 (as 15/8 < 2) */
    def fullBytes: Bytes = (toBits.toLong / 8).bytes


    /** For easier implementation of specializations. */
    protected def nextPowerLong(value: Long): Long = {
      @tailrec
      def nextPowFrom(value: Long, power: Long): Long =
        if( power >= value ) { power } else { nextPowFrom(value, power << 1) }

      if( value == 0L ) {
        value
      } else {
        nextPowFrom(value, 1L)
      }
    }

    /** Calculates next power of two (2 pow N) no less than the caller. */
    def nextPowerBits: Bits = nextPowerLong(toBits.toLong).bits

    /** Whenever it is a power of two (2 pow N). */
    def isPower: Boolean = this.toBits == this.nextPowerBits


    /** General addition in bits */
    def +[M <: MemoryUnit](other: M): Bits = (toBits.toLong + other.toBits.toLong).bits

    /** General subtraction in bits */
    def -[M <: MemoryUnit](other: M): Bits = (toBits.toLong - other.toBits.toLong).bits

    /** How many blocks would fit here, e.g. 4.bytes / 3.bits == 5 (as 5 <= 16/3 < 6) */
    def /[M <: MemoryUnit](other: M): Long = toLong / other.toLong

    /** Bit shift from full block, e.g. 4.bytes / 3.bits == 1 (as 16 - (16/3)*3 == 1) */
    def %[M <: MemoryUnit](other: M): Bits = (toLong % other.toLong).bits

    def compare(other: MemoryUnit): Int = toBits.toLong.compare(other.toBits.toLong)
  }

  /** Full bytes memory unit. Can be safely turned to bytes. */
  sealed trait FullBytes extends Any with MemoryUnit {
    /** Turn to bytes safely. */
    def toBytes: Bytes

    override def fullBytes: Bytes = toBytes
    override def tryBytes = Some(toBytes)
    override def toBits: Bits = (toBytes.toLong * 8).bits

    def nextPowerBytes: Bytes = nextPowerLong(toBytes.toLong).bytes
    override def isPower: Boolean = toBytes == nextPowerBytes

    /** Alignment if is power, None otherwise. */
    def tryAlignment: Option[Alignment] = if( toLong > 0 && isPower ) { Some(toAlignment) } else { None }

    /** Alignment conversion, unsafe */
    def toAlignment = Alignment(toBytes)

    override def toString = toBytes.toString
  }

  /** Bits value.
    *
    * @note Being AnyVal, can be treated like a primitive.
    * */
  case class Bits(value: Long) extends AnyVal with MemoryUnit {
    def toLong = value
    def toBits = this
    def tryBytes: Option[Bytes] = if( value % 8 == 0 ) { Some((value / 8).bytes) } else { None }

    def nextPower: Bits = nextPowerBits

    override def toString = s"$value.bits"
  }

  /** Bytes value.
    *
    * @note Being AnyVal, can be treated like a primitive.
    * */
  case class Bytes(value: Long) extends AnyVal with FullBytes {
    def toLong = value
    def toBytes = this


    /** Specialized addition which maintain precision. */
    def +(other: Bytes) = new Bytes(toLong + other.toLong)

    /** Specialized subtraction which maintain precision. */
    def -(other: Bytes) = new Bytes(toLong - other.toLong)

    /** Specialized shift which maintain precision. */
    def %(other: Bytes) = new Bytes(toLong % other.toLong)

    def nextPower: Bytes = nextPowerBytes

    override def toString = f"0x$value%X.bytes"
  }

  /** Memory units-related Numeric types utilities. */
  implicit class asMemoryUnits(val value: Long) extends AnyVal {
    /** Convert to bits. */
    def bits = Bits(value)

    /** Convert to bytes */
    def bytes = Bytes(value)

    /** Convert to word length in words */
    def words: Bytes = (WordSize.toLong / 8).bytes
  }



  /** 2 pow N bytes (runtime check) adapter. Can't be a value class so it can have a requirement. */
  case class Alignment private (value: Long) extends Ordered[Alignment] {
    def toLong: Long = value
    def toBytes: Bytes = value.bytes
    def toBits: Bits = toBytes.toBits

    def compare(other: Alignment): Int = toLong.compare(other.toLong)

    override def toString: String = "Alignment(" + toBytes + ")"
  }
  object Alignment {
    /** Simple constructor from full bytes. */
    def apply[F <: FullBytes](fbytes: F): Alignment = {
      val value = fbytes.toBytes.toLong
      require( value > 0L )
      require( value.bytes.isPower )
      Alignment(value)
    }

    /** Simple extractor to bytes */
    def unapply(align: Alignment): Option[Bytes] = Some(align.toBytes)
  }


  /** Addressing kind. Can be:
    *  * Compact --- aligned to accesswidth
    *  * Regalign --- aligned to regalign
    *  * Fullalign --- like regalign, but arrays aligned as a whole
    * */
  object Addressing extends Enumeration {
    type Addressing = Value
    val Regalign, Compact, Fullalign = Value

    /** Default value (regalign) */
    val default = Regalign
  }


  // hardcodded as for now
  //TODO: generalize
  /** Machine word size */
  val WordSize = 64

  /** Address in memory. Word-aware. */
  case class Address private (offset: Long) extends Ordered[Address] {

    /** Address moved by memory shift. */
    def +[M <: MemoryUnit](munit: M): Address = Address(toBits + munit)

    /** Address moved by memory shift. */
    def -[M <: MemoryUnit](munit: M): Address = Address(toBits - munit)

    /** Shift between addresses. */
    def -(other: Address): Bits = toBits - other.toBits

    /** Shift from bit block alignment. */
    def %[M <: MemoryUnit](munit: M): Bits = toBits % munit

    /** Align to any block. */
    def alignTo(align: Alignment): Address = {
      val bitAlign = align.toBits
      val shift = this % bitAlign
      if (shift.toLong == 0) { this } else { this + (bitAlign - shift) }
    }

    def compare(other: Address): Int = offset.compare(other.offset)

    /** Erasure word info, return bits. */
    def toBits: Bits = offset.bits

    /** Erasure word info, try bytes. */
    def tryBytes: Option[Bytes] = toBits.tryBytes

    /** Erasure word info, full bytes. */
    def fullBytes: Bytes = toBits.fullBytes

    /** Full words */
    def words: Long = (toBits.toLong / WordSize)

    /** Bit offset from full words */
    def bits: Bits = (toBits.toLong % WordSize).bits

    override def toString = s"Address at ($fullBytes + $bits)"
  }
  object Address {
    /** From raw memory */
    def apply[M <: MemoryUnit](munit: M): Address = {
      require(munit.toLong >= 0)
      Address(munit.toBits.toLong)
    }

    /** From word number and bit offset.
      *
      * @note It's valid for bit offset to be negative or bigger than word size.
      */
    def apply(word: Long, bits: Bits): Address = Address((word * WordSize).bits + bits)

    /** Syntactic sugar.
      *
      * @example Address at (x.words + y.bits)
      */
    def at[M <: MemoryUnit](munit: M) = Address(munit)

    /** Simple extractor */
    def unapply(address: Address): Option[(Long, Bits)] = Some((address.words, address.bits))
  }

  /** Extractor for Address.
    *
    * @example
    *   sth match {
    *     case Address at Bits(offset) => offset
    *   }
    */
  object at {
    def unapply(address: Address): Option[(Address.type, Bits)] = Some((Address, address.toBits))
  }


  /** Contiguous range of addresses.
    *
    * @param pos starting poing of the range
    * @param width width in bits
    * */
  case class AddressRange private (pos: Address, width: Bits) {
    require(width.toLong > 0L)  // pos <= lim

    /** First address free after this range,
      * e.g. AddressRange(Address(32.bytes), 8.bytes).lim == Address(40.bytes)
      */
    def lim: Address = pos + width

    /** Position and the next position after the range,
      * e.g. AddressRange(Address(32.bytes), 8.bytes).toAddressPair == (Address(32.bytes), Address(40.bytes)) */
    def toAddressPair: (Address, Address) = (pos, lim)

    override def toString: String = "AddressRange(pos: " + pos + ", lim: " + lim + ")"

    /** Highly friendly and readable range form */
    def rangeString: String = "" + pos.toBits.toLong + ".." + lim.toBits.toLong
  }
  object AddressRange {
    /** Simple explicit constructor */
    def apply[M <: MemoryUnit](addr: Address, munit: M) = new AddressRange(addr, munit.toBits)

    /** Construct from two addresses. */
    def apply(pos: Address, lim: Address): AddressRange = AddressRange(pos, (lim-pos).toBits)

    /** Field constructor */
    def makeField[M <: MemoryUnit](addr: Address, munit: M) = AddressRange(addr, munit.toBits)

    /** Register constructor.
      *
      * Arguments' meaning and relation to addressing --- @see Addressing
      */
    def placeReg(at: Address,
                 regwidth: Alignment,
                 alignment: Option[Alignment] = None,
                 accesswidth: Option[Alignment] = None,
                 addressing: Addressing.Value = Addressing.default): AddressRange = {
      // 1. addressing is compact => use accesswidth (otherwise use regwidth)
      // 2. default accesswidth == regwidth
      // 3. if explicit alignment is specified, use it instead
      // 4. accesswidth must be no bigger than regwidth
      accesswidth.foreach(x => require(x <= regwidth))
      val width = accesswidth
        .filter(_ => addressing == Addressing.Compact)
        .getOrElse(regwidth)
      val align = alignment.getOrElse(width)
      val lo = at alignTo align
      AddressRange(lo, regwidth.toBits)
    }
  }


  /** Some handy conversions */
  object ImplicitConversions {
    /** Conversion to option for memory units. Works with indirect conversions too. */
    implicit def muToOption[A, M <: MemoryUnit](value: A)(implicit f: A => M) = Some(f(value))

    /** Conversion to option for alignment. Works with indirect conversions too. */
    implicit def alToOption[A](value: A)(implicit f: A => Alignment) = Some(f(value))

    /** Conversion to alignment for full bytes memory units. Works with indirect conversions too. */
    implicit def muToAl[A, M <: FullBytes](value: A)(implicit f: A => M): Alignment = f(value).toAlignment

    /** Conversion to addressing from option. Works with indirect conversions too. */
    implicit def optionToAddressing[A](value: A)(implicit f: A => Option[Addressing.Value]): Addressing.Value =
      f(value).getOrElse(Addressing.default)
  }


  /** Lifting and unlifting memory-related types. */
  trait LiftableMemoryImpl {
    val universe: Universe
    import universe._

    lazy val asMemoryUnitsSym: TypeSymbol = symbolOf[asMemoryUnits]
    lazy val BitsCom = symbolOf[Bits].companion
    lazy val BytesCom = symbolOf[Bytes].companion

    implicit lazy val liftBits = Liftable[Bits] { b =>
      q"(new $asMemoryUnitsSym(${b.toLong})).bits"
    }
    implicit lazy val unliftBits = Unliftable[Bits] {
      case q"${value: Long}.bits" => value.bits
      case q"${value: Int}.bits" => value.toLong.bits
      case q"$sym(${value: Long})" if sym == BitsCom => value.bits
      case q"$sym(${value: Int})" if sym == BitsCom => value.toLong.bits
    }

    implicit lazy val liftBytes = Liftable[Bytes] { b =>
      q"(new $asMemoryUnitsSym(${b.value})).bytes"
    }
    implicit lazy val unliftBytes = Unliftable[Bytes] {
      case q"${value: Long}.bytes" => value.bytes
      case q"${value: Int}.bytes" => value.toLong.bytes
      case q"$sym(${value: Long})" if sym == BytesCom => value.bytes
      case q"$sym(${value: Int})" if sym == BytesCom => value.toLong.bytes
    }

    implicit lazy val unliftMemoryUnit = Unliftable[MemoryUnit] {
      case q"${bits: Bits}" => bits
      case q"${bytes: Bytes}" => bytes
    }
    implicit lazy val unliftFullBytes = Unliftable[FullBytes] {
      case q"${bytes: Bytes}" => bytes
    }

    lazy val AlignmentCom = symbolOf[Alignment].companion
    implicit lazy val liftAlignment = Liftable[Alignment] { a =>
      q"$AlignmentCom(${a.toBytes})"
    }
    implicit lazy val unliftAlignment = Unliftable[Alignment] {
      case q"$sym(${fbytes: FullBytes})" if sym == AlignmentCom => Alignment(fbytes.toBytes)
      case q"${fbytes: FullBytes}.toAlignment" => fbytes.toAlignment
    }
  }

  object RuntimeLiftableMemory extends LiftableMemoryImpl {
    type U = runtimeUniverse.type
    val universe: U = runtimeUniverse
  }
  trait LiftableMemory extends LiftableMemoryImpl {
    val c: blackbox.Context  // whitebox's one is blackbox's one's subtype
    type U = c.universe.type
    val universe: U = c.universe
  }
}
