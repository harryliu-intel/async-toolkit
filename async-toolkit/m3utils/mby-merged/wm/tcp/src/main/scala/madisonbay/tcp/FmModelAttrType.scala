package madisonbay.tcp

import java.io.EOFException

//scalastyle:off magic.number
object FmModelAttrType extends Enumeration {
  val GetRequest = Value(1, "GetRequest")
  val GetResponse = Value(2, "GetResponse")
  val Set = Value(3, "Set")
  val SetAck = Value(4, "SetAck")

  implicit val encoder: ByteArrayEncoder[FmModelAttrType.Value] =
    fmat => ByteArrayEncoder.u8bae.encode(fmat.id.byteValue())
  implicit val bitSize: BitSize[FmModelAttrType.Value] = BitSize.bitSizeOf(8)
  implicit val decoder: ByteArrayDecoder[FmModelAttrType.Value] =
    ByteArrayDecoder.decoder(new EOFException, a => FmModelAttrType(a(0)))
}
