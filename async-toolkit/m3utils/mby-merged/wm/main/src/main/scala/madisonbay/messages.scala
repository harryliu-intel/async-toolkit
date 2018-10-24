package madisonbay

import madisonbay.tcp._

package object messages {

  sealed trait Message
  case class EgressSocketInfo(hostname: String, port: Int) extends Message
  case class IosfRegBlkWrite(iosff: IosfRegBlkWriteReqHdr) extends Message
  case class IosfRegRead(iosf: IosfRegReadReq) extends Message
  case class IosfRegWrite(iosf: IosfRegWriteReq) extends Message
  case class Packet(i: Int) extends Message
  case object NotSupported extends Message
  case object Quit extends Message

}
