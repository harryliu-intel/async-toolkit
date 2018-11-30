package madisonbay.memory


/** Bytes value.
  *
  * @note Being AnyVal, can be treated like a primitive.
  * */
case class Bytes(value: Long) extends AnyVal with FullBytes {
  def toLong: Long = value
  def toBytes: Bytes = this

  /** Specialized addition which maintain precision. */
  def +(other: Bytes): Bytes = Bytes(toLong + other.toLong)

  /** Specialized subtraction which maintain precision. */
  def -(other: Bytes): Bytes = Bytes(toLong - other.toLong)

  /** Specialized shift which maintain precision. */
  def %(other: Bytes): Bytes = Bytes(toLong % other.toLong)

  def nextPower: Bytes = nextPowerBytes

  override def toString: String = f"0x$value%X.bytes"
}
