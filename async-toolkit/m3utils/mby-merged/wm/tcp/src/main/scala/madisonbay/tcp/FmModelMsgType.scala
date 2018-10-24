//scalastyle:off
package madisonbay.tcp

import java.io.EOFException
import java.nio.ByteBuffer

object FmModelMsgType extends Enumeration {
  val Packet = Value(0, "Packet")
  val LinkState = Value(1, "LinkState")
  val SwitchState = Value(2, "SwitchState")
  val SetEgressInfo = Value(3, "SetEgressInfo")
  val EnableAlternativeDataPath = Value(4, "EnableAlternativeDataPath")
  val PacketLoopback = Value(5, "PacketLoopback")
  val PacketEot = Value(6, "PacketEot")
  val Mgmt = Value(7, "Mgmt")
  val Attr = Value(8, "Attr")
  val GetInfo = Value(9, "GetInfo")
  val Error = Value(10, "Error")
  val Iosf = Value(11, "Iosf")
  val Ctrl = Value(12, "Ctrl")
  val VersionInfo = Value(13, "VersionInfo")
  val NvmRead = Value(14, "NvmRead")
  val CommandQuit = Value(15, "CommandQuit")

  implicit val bitSize: BitSize[FmModelMsgType.Value] = BitSize.bitSizeOf(16)
  implicit val bae: ByteArrayEncoder[FmModelMsgType.Value] =
    fmmmt => ByteArrayEncoder.u16bae.encode(fmmmt.id.byteValue())
  implicit val bad: ByteArrayDecoder[FmModelMsgType.Value] =
    ByteArrayDecoder.decoder(new EOFException(), a =>
      FmModelMsgType(ByteBuffer.wrap(a).getShort())
    )
}
