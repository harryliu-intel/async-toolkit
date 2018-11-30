package madisonbay.memory


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
    require(value > 0L)
    require(value.bytes.isPower)
    Alignment(value)
  }

  /** Simple extractor to bytes */
  def unapply(align: Alignment): Option[Bytes] = Some(align.toBytes)
}
