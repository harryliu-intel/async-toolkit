package madisonbay.tcp

// TODO: not used!
//scalastyle:off magic.number
object FmModelDataType extends Enumeration {
  val Length = 1;

  val Packet = Value(160, "Packet")
  val SbId = Value(161, "SbId")
  val SbTc = Value(162, "SbTc")
  val PacketMeta = Value(163, "PacketMeta")

  implicit val encoder: ByteArrayEncoder[FmModelDataType.Value] =
    value => ByteArrayEncoder.u8bae.encode(value.id.byteValue())
}
