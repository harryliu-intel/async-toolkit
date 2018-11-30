package madisonbay
package fs2app
package algebra

import madisonbay.tcp._
import scalaz.StateT

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

  trait MessageHandler[F[_],A] {

    type Transition = StateT[F,A,Array[Byte]]

    def quit: F[Unit]
    def notSupported: F[Unit]
    def packets(p: Packets): Transition
    def regWrite(i: IosfRegWrite): Transition
    def regRead(i: IosfRegRead): Transition
    def regBlkWrite(i: IosfRegBlkWrite): Transition
    def egressSocketInfo(esi: EgressSocketInfo): F[Unit]

  }

  object MessageHandler {
    def apply[F[_],A](implicit mh: MessageHandler[F,A]): MessageHandler[F,A] = mh
  }
}
