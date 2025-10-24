package madisonbay.tcp

import java.io.EOFException

//scalastyle:off magic.number
object FmModelDataType extends Enumeration {
  val Length = 1

  val Packet = Value(160, "Packet")
  val SbId = Value(161, "SbId")
  val SbTc = Value(162, "SbTc")
  val PacketMeta = Value(163, "PacketMeta")

  implicit val encoder: ByteArrayEncoder[FmModelDataType.Value] =
    value => ByteArrayEncoder.u8bae.encode(value.id.byteValue())
  implicit val bitSize: BitSize[FmModelDataType.Value] = BitSize.bitSizeOf(8)
  implicit val decoder: ByteArrayDecoder[FmModelDataType.Value] =
    ByteArrayDecoder.decoder(new EOFException(), a => FmModelDataType(a(0).toInt & 0xff))
}
