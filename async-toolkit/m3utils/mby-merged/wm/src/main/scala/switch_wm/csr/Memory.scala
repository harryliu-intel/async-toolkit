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
}