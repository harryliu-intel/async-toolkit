package madisonbay
package fs2app

import madisonbay.csr._
import madisonbay.tcp._
import madisonbay.tcp.iosf._
import madisonbay.logger.Logger
import madisonbay.fs2app.algebra.messages._
import scalaz.{ Kleisli, StateT }
import scalaz.MonadError
import scalaz.MonadState
import scalaz.syntax.all._
import scalaz.std.list._
import shapeless.Witness

import java.nio.ByteBuffer

import com.intel.cg.hpfd.madisonbay.PrimitiveTypes._
import com.intel.cg.hpfd.madisonbay.Memory._
import com.intel.cg.hpfd.madisonbay.BitVector

class Fs2DefaultMessageHandler[
  F[_]: λ[G[_] => MonadError[G,Throwable]]: Logger,
  Root
] extends MessageHandler[F,CsrContext[Root]] {

  val logger = Logger[F]
  val ME = MonadError[F,Throwable]
  val MS = StateT.stateTMonadState[CsrContext[Root],F]

  val noop: Transition = StateT(init => ME.point(init,Array.empty[Byte]))

  def registerNotFoundAt[G[_],A](address: Address)(implicit GME: MonadError[G,Throwable]): G[A] =
    GME.raiseError[A](new Throwable(s"Register with [$address] not found!"))

  def merge[A <: Int, B <: Int, C <: Int](a: BitField[A], b: BitField[B], c: BitField[C])
    (implicit aw: Witness.Aux[A], bw: Witness.Aux[B]): Long =
    a.value | (b.value << aw.value) | (c.value << (aw.value + bw.value))

  def quit: F[Unit] = logger.debug("Terminating program! Bye bye!")
  def notSupported: F[Unit] = ME.point(())
  def egressSocketInfo(esi: EgressSocketInfo): F[Unit] =
    logger.trace(s"Queuing egress socket info: $esi")
  def packets(p: Packets): Transition = noop
  def regWrite(i: IosfRegWrite): Transition = noop

  def regRead(i: IosfRegRead): Transition = {
    // doesn't compile when S is moved to implicit parameter list... :/
    // https://github.com/scalaz/scalaz/issues/2024
    def program[G[_]: Logger](S: MonadState[G,CsrContext[Root]])(implicit GME: MonadError[G,Throwable]): G[Array[Byte]] =
      for {
        init                   <- S.get
        CsrContext(root, paths) = init
        IosfRegRead(req)        = i
        address                 = Address at (merge(req.addr0, req.addr1, req.addr2) bytes)
        _                      <- Logger[G].trace(
          s"Trying to read register at address [$address] based on $i"
        )
        value                  <- paths.get(address)
          .flatMap(_.getOption(root))
          .map(_.extract[Long])
          .fold(registerNotFoundAt[G,Long](address))(l => GME.pure(l))
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
      } yield ByteArrayEncoder[FmModelMessageHdr].encode(header) ++
        ByteArrayEncoder[IosfRegCompDataHdr].encode(iosfData) ++
        ByteArrayEncoder[U64].encode(value)

    program[StateT[F,CsrContext[Root],?]](MS)
  }

  // TODO: write tailRec processing for block data with decode[F]
  def regBlkWrite(i: IosfRegBlkWrite): Transition = {
    val IosfRegBlkWrite(req,data) = i
    val registerBytesLength = BitSize[U64].getBytes
    val startingAddress = merge(req.addr0, req.addr1, req.addr2)
    val addressesChunks =
      Stream.iterate(Address at startingAddress.bytes)(_ + registerBytesLength.bytes)
    val dataChunks = Stream(
      data
        .grouped(registerBytesLength)
        .filter(_.size == registerBytesLength)
        .map(arr => ByteBuffer.wrap(arr).getLong)
        .map(BitVector(_))
        .toSeq: _*
    )
    val msgSize = BitSize[FmModelMessageHdr].getBytes + BitSize[IosfRegCompNoData].getBytes
    val header = FmModelMessageHdr(msgSize, 2, FmModelMsgType.Iosf, 0, 0)
    val iosfNoData = IosfRegCompNoData(
      sai = BitField(1),
      dest = req.source,
      source = req.dest,
      tag = req.tag,
      rsp = BitField(0),
      rsvd0 = BitField(0)
    )
    val responseArray = ByteArrayEncoder[FmModelMessageHdr].encode(header) ++
      ByteArrayEncoder[IosfRegCompNoData].encode(iosfNoData)

    def kleisliT[G[_]: Logger: scalaz.Monad](S: MonadState[G,CsrContext[Root]])
      (chunk: BitVector, address: Address)
      (implicit GME: MonadError[G,Throwable]): Kleisli[G,Unit,Unit] = Kleisli.kleisli(_ =>
      for {
        init                   <- S.get
        _                      <- Logger[G].trace(s"Writing [$chunk] data into [$address].")
        CsrContext(root, paths) = init
        nextRoot               <- paths.get(address).fold(registerNotFoundAt[G,Root](address)) {
          _.modifyF(_ => chunk.point[G])(root)
        }
        _                      <- S.put(CsrContext(nextRoot,paths))
      } yield ()
    )

    (dataChunks zip addressesChunks).toList.traverseKTrampoline(
      (kleisliT[StateT[F,CsrContext[Root],?]](MS)_).tupled
    ).run(()).map(_ => responseArray)
  }
}
