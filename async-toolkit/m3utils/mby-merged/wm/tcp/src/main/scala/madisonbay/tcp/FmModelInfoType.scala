package madisonbay.tcp

import java.io.EOFException

object FmModelInfoType extends Enumeration {
  val Request = Value(1, "Request")
  val Response = Value(2, "Response")

  implicit val encoder: ByteArrayEncoder[FmModelInfoType.Value] =
    value => ByteArrayEncoder.u8bae.encode(value.id.byteValue())
  implicit val bitSize: BitSize[FmModelInfoType.Value] = {
    val eightBits = 8
    BitSize.bitSizeOf(eightBits)
  }
  implicit val decoder: ByteArrayDecoder[FmModelInfoType.Value] =
    ByteArrayDecoder.decoder(new EOFException, a => FmModelInfoType(a(0)))
}
