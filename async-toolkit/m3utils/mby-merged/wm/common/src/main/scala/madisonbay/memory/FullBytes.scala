package madisonbay.memory


/** Full bytes memory unit. Can be safely turned to bytes. */
trait FullBytes extends Any with MemoryUnit {
  /** Turn to bytes safely. */
  def toBytes: Bytes

  override def fullBytes: Bytes = toBytes
  override def tryBytes: Option[Bytes] = Some(toBytes)
  override def toBits: Bits = (toBytes.toLong * 8).bits

  def nextPowerBytes: Bytes = nextPowerLong(toBytes.toLong).bytes
  override def isPower: Boolean = toBytes == nextPowerBytes

  /** Alignment if is power, None otherwise. */
  def tryAlignment: Option[Alignment] = if (toLong > 0 && isPower) { Some(toAlignment) } else { None }

  /** Alignment conversion, unsafe */
  def toAlignment: Alignment = Alignment(toBytes)

  override def toString: String = toBytes.toString
}