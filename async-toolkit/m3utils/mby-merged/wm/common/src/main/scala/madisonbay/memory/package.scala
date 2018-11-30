package madisonbay


package object memory {
  // hardcodded as for now
  //TODO: generalize
  /** Machine word size */
  val WordSize = 64

  /** Memory units-related Numeric types utilities. */
  implicit class asMemoryUnits(val value: Long) extends AnyVal {
    /** Convert to bits. */
    def bits = Bits(value)

    /** Convert to bytes */
    def bytes = Bytes(value)

    /** Convert to word length in words */
    def words: Bytes = (WordSize.toLong / 8).bytes
  }
}
