package madisonbay
package fs2app

import madisonbay.tcp._
import madisonbay.tcp.iosf._
import madisonbay.logger.Logger
import madisonbay.fs2app.algebra.messages._

import scalaz.StateT
import scalaz.MonadError
import scalaz.syntax.all._

import com.intel.cg.hpfd.madisonbay.PrimitiveTypes._

class Fs2DefaultMessageHandler[
  F[_]: Lambda[G[_] => MonadError[G,Throwable]]: Logger,
  A
] extends MessageHandler[F,A] {

  val logger = Logger[F]
  val me = MonadError[F,Throwable]

  val noop: Transition = StateT(init => me.point((init,Array.empty[Byte])))

  def quit: F[Unit] = logger.debug("Terminating program! Bye bye!")
  def notSupported: F[Unit] = me.point(())
  def egressSocketInfo(esi: EgressSocketInfo): F[Unit] =
    logger.trace(s"Queuing egress socket info: $esi")
  def packets(p: Packets): Transition = noop
  def regWrite(i: IosfRegWrite): Transition = noop

  def regRead(i: IosfRegRead): Transition = {
    val IosfRegRead(req) = i
    StateT(init =>
      for {
        _ <- logger.debug(s"Received $i")
        header = FmModelMessageHdr(
          BitSize[FmModelMessageHdr].getBytes
            + BitSize[IosfRegCompDataHdr].getBytes
            + BitSize[U64].getBytes,
          2.shortValue(),
          FmModelMsgType.Iosf,
          0.shortValue,
          0.shortValue
        )
        iosfData = IosfRegCompDataHdr(
          sai = BitField(1),
          dest = req.source,
          source = req.dest,
          tag = req.tag,
          rsp = BitField(0),
          rsvd0 = BitField(0)
        )
        registerValue = 0L
        array = ByteArrayEncoder[FmModelMessageHdr].encode(header) ++
        ByteArrayEncoder[IosfRegCompDataHdr].encode(iosfData) ++
        ByteArrayEncoder[U64].encode(registerValue)
        _ <- logger.debug(s"Sending back hardcoded register value: $registerValue")
      } yield (init, array)
    )
  }

  def regBlkWrite(i: IosfRegBlkWrite): Transition = {
    val IosfRegBlkWrite(req,_) = i
    StateT(init =>
      for {
        _ <- logger.debug(s"Received IosfRegBlkWrite: $i")
        _ <- logger.debug("Sending back dummy response without any internal processing.")
        header = FmModelMessageHdr(
          BitSize[FmModelMessageHdr].getBytes + BitSize[IosfRegCompNoData].getBytes,
          2.shortValue(),
          FmModelMsgType.Iosf,
          0.shortValue,
          0.shortValue
        )
        iosfNoData = IosfRegCompNoData(
          sai = BitField(1),
          dest = req.source,
          source = req.dest,
          tag = req.tag,
          rsp = BitField(0),
          rsvd0 = BitField(0)
        )
        array = ByteArrayEncoder[FmModelMessageHdr].encode(header) ++
        ByteArrayEncoder[IosfRegCompNoData].encode(iosfNoData)
      } yield (init, array)
    )
  }
}
