package switch_wm.csr

import scala.reflect.ClassTag
import scala.annotation.tailrec


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
    def tryAlignment: Option[Alignment] = if( isPower ) { Some(toAlignment) } else { None }

    /** Alignment conversion, unsafe */
    def toAlignment = Alignment(toBytes.toLong)
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
  }


  /** Memory units-related Numeric types utilities. */
  implicit class asMemoryUnits(val value: Long) extends AnyVal {
    /** Convert to bits. */
    def bits = Bits(value)

    /** Convert to bytes */
    def bytes = Bytes(value)
  }


  /** 2 pow N bytes (runtime check) adapter. Can't be a value class so it can have a requirement. */
  case class Alignment(value: Long) extends  Ordered[Alignment] {
    require( value.bytes.isPower )
    def toLong = value
    def toBytes = toLong.bytes
    def toBits = toBytes.toBits

    def compare(other: Alignment): Int = toLong.compare(other.toLong)
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
  class Address(val word: Long, val bit: Long) extends Ordered[Address] {
    /** Address moved by memory shift. */
    def +[M <: MemoryUnit](munit: M): Address = {
      val allbits = (bit + munit.toBits.toLong)
      Address(word + allbits / WordSize, allbits % WordSize)
    }

    /** Address moved by memory shift. */
    def -[M <: MemoryUnit](munit: M): Address = {
      val allbits = (bit - munit.toBits.toLong)
      val a = word + allbits / WordSize
      val b = WordSize - (allbits % WordSize)
      if( b == WordSize ) { Address(word + a, 0) }
      else { Address(word + a - 1, b) }
    }

    /** Shift between addresses. */
    def -(other: Address): Bits = ((word - other.word) * WordSize + (bit - other.bit)).bits

    /** Shift from bit block alignment. */
    def %[M <: MemoryUnit](munit: M): Bits = {
      val bits = munit.toBits.toLong
      val wom = word % bits
      val bam = WordSize % bits
      val bim = bit % bits
      ((wom * bam + bim) % bits).bits
    }

    /** Align to any block. */
    def modAlign[M <: FullBytes](align: M): Address = {
      val tobits = Address(if(bit != 0) { word + 1 } else { word }, 0)
      val shift = (align - (tobits % align)) % align
      this + shift
    }

    /** Align to proper block of size 2 pow N. */
    def modAlign(align: Alignment): Address = modAlign(align.toBytes)

    def compare(that: Address): Int =
      (word - that.word).signum match {
        case 0 => (bit - that.bit).signum
        case sign @ _ => sign
      }

    /** Erasure word info, return bits. */
    def toBits: Bits = ((word * WordSize) + bit).bits

    /** Erasure word info, try bytes. */
    def tryBytes: Option[Bytes] = toBits.tryBytes

    /** Erasure word info, full bytes. */
    def fullBytes: Bytes = toBits.fullBytes
  }
  object Address {
    /** Simple explicit constructor */
    def apply(word: Long, bit: Long): Address = new Address(word, bit)

    /** From raw memory in bits */
    def apply(bits: Bits): Address = Address(bits.value / WordSize, bits.value % WordSize)

    /** From raw memory */
    def apply[M <: MemoryUnit](munit: M): Address = Address(munit.toBits)
  }


  /** Contiguous range of addresses.
    *
    * @param pos starting poing of the range
    * @param width width in bits
    * */
  class AddressRange(val pos: Address, val width: Bits) {
    /** First address free after this range,
      * e.g. AddressRange(Address(32.bytes), 8.bytes).lim == Address(40.bytes)
      */
    def lim: Address = pos + width

    /** Position and the next position after the range,
     * e.g. AddressRange(Address(32.bytes), 8.bytes).toAddressPair == (Address(32.bytes), Address(40.bytes)) */
    def toAddressPair = (pos, lim)
  }
  object AddressRange {
    /** Simple explcit constructor */
    def apply[M <: MemoryUnit](addr: Address, munit: M) = new AddressRange(addr, munit.toBits)

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
      val lo = at.modAlign(align)
      AddressRange(lo, (lo + regwidth.toBytes).toBits)
    }
  }
}