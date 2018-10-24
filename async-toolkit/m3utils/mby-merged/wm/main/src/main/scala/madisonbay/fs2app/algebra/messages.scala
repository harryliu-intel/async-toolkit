package madisonbay
package fs2app
package algebra

import madisonbay.tcp._

package object messages {

  sealed trait Message
  case class EgressSocketInfo(hostname: String, port: Int) extends Message
  case class IosfRegBlkWrite(iosf: IosfRegBlkWriteReqHdr, blkData: Array[Byte]) extends Message
  case class IosfRegRead(iosf: IosfRegReadReq) extends Message
  case class IosfRegWrite(iosf: IosfRegWriteReq) extends Message
  case class Packet(port: Short, content: Array[Byte])
  case class Packets(items: List[Packet]) extends Message
  case object NotSupported extends Message
  case object Quit extends Message

}
