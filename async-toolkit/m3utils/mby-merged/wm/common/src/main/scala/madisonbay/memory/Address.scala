package madisonbay.memory


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

  override def toString: String = s"Address at ($fullBytes + $bits)"
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
  def at[M <: MemoryUnit](munit: M): Address = Address(munit)

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