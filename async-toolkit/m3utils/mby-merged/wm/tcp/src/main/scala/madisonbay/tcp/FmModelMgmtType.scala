package madisonbay.tcp

import java.io.EOFException

//scalastyle:off magic.number
object FmModelMgmtType extends Enumeration {
  val ReadRequest = Value(1, "ReadRequest")
  val ReadResponse = Value(2, "ReadResponse")
  val Write = Value(3, "Write")
  val WriteAck = Value(4, "WriteAck")
  val Read64Request = Value(5, "Read64Request")
  val Read64Response = Value(6, "Read64Response")
  val Write64 = Value(7, "Write64")
  val Write64Ack = Value(8, "Write64Ack")

  implicit val size: BitSize[FmModelMgmtType.Value] = BitSize.bitSizeOf(8)
  implicit val bae: ByteArrayEncoder[FmModelMgmtType.Value] =
    fmmmt => ByteArrayEncoder.u8bae.encode(fmmmt.id.byteValue())
  implicit val bad: ByteArrayDecoder[FmModelMgmtType.Value] =
    ByteArrayDecoder.decoder(new EOFException(), a => FmModelMgmtType(a(0)))
}
