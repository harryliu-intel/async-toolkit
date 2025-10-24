package madisonbay.memory


/** Bits value.
  *
  * @note Being AnyVal, can be treated like a primitive.
  * */
case class Bits(value: Long) extends AnyVal with MemoryUnit {
  def toLong: Long = value
  def toBits: Bits = this
  def tryBytes: Option[Bytes] = if (value % 8 == 0) { Some((value / 8).bytes) } else { None }

  def nextPower: Bits = nextPowerBits

  override def toString: String = s"$value.bits"
}
