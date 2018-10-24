package madisonbay
package fs2app

import fs2._

import scalaz.Monad
import scalaz.MonadError
import scalaz.StateT
import scalaz.syntax.all._

import madisonbay.logger.Logger
import madisonbay.tcp._
import madisonbay.tcp.iosf._
import madisonbay.fs2app.algebra.messages._

import com.intel.cg.hpfd.madisonbay.PrimitiveTypes._

package object deserialization {
  // DST stands for 'Deserialization StateT'
  type DST[Effect[_],Out] = StateT[Effect,Array[Byte],Out]

  val headerDecoder = ByteArrayDecoder[FmModelMessageHdr]

  // doesn't work :/
  // val S = scalaz.StateT.stateTMonadState[Array[Byte],F]
  // import S._ // for 'put' and 'get' functions on StateT
  // so this is the reason why this class is defined here...
  class StateTExtensions[F[_]: Monad,S] {
    def getS: StateT[F,S,S] = StateT(s => (s,s).point[F])
    def putS(newS: S): StateT[F,S,Unit] = StateT(_ => (newS,()).point[F])
    def liftS[A](fu: F[A]): StateT[F,S,A] = StateT(s => fu.map(a => (s,a)))
  }

  def notSupported[F[_]: Monad]: DST[F,NotSupported.type] = NotSupported.point[DST[F,?]]
  def quit[F[_]: Monad]: DST[F,Quit.type] = Quit.point[DST[F,?]]

  def iosf[F[_]](implicit me: MonadError[F,Throwable]): DST[F,Message] = {
    val S = new StateTExtensions[F,Array[Byte]]
    import S._

    val regBlkWriteReqDec = ByteArrayDecoder[IosfRegBlkWriteReqHdr]
    val regReadReqDec = ByteArrayDecoder[IosfRegReadReq]
    val regWriteReqDec = ByteArrayDecoder[IosfRegWriteReq]

    for {
      initial  <- getS
      header   <- regBlkWriteReqDec.decode[F]
      blkBytes <- getS
      _        <- putS(initial)
      msg      <- header.opcode.value match {
        case RegBlkWrite => IosfRegBlkWrite(header, blkBytes).point[DST[F,?]]
        case RegRead     => regReadReqDec.decode[F].map(IosfRegRead)
        case RegWrite    => regWriteReqDec.decode[F].map(IosfRegWrite)
        case _           => notSupported[F]
      }
    } yield msg
  }

  def egressSocketInfo[F[_]](implicit me: MonadError[F,Throwable]): DST[F,EgressSocketInfo] = {
    val S = new StateTExtensions[F,Array[Byte]]
    import S._

    for {
      egressInfoHdr <- ByteArrayDecoder[FmModelSetEgressInfoHdr].decode[F]
      furtherBytes  <- getS
      hostname      =  furtherBytes.map(_.toChar).mkString
      port          =  egressInfoHdr.Tcpport.toInt & 0xffff
    } yield EgressSocketInfo(hostname, port)
  }

  def packet[F[_]: Logger](port: U16)(implicit me: MonadError[F,Throwable]): DST[F,Packets] = {
    val S = new StateTExtensions[F,Array[Byte]]
    import S._
    val logger = Logger[F]

    lazy val result: DST[F,Packets] = for {
      dataType       <- ByteArrayDecoder[FmModelDataType.Value].decode[F]
      length         <- ByteArrayDecoder[U32].decode[F]
      bytes          <- getS
      (content, next) = bytes.splitAt(length)
      packet          = Packet(port, content)
      items          <- liftS {
        import FmModelDataType._
        lazy val continuation = result.run(next)
        (dataType, next.isEmpty) match {
          case (PacketMeta, false) => continuation.map { case (_,p) => packet :: p.items }
          case (PacketMeta, true) => (packet :: Nil).point[F]
          case (other, false) =>
            for {
              _     <- logger.warn(s"Not supported packet [$other]. Will be skipped!")
              _     <- logger.warn(s"For now only ${PacketMeta} is supported.")
              _     <- logger.warn(s"Skipping ->> [${content.size}] <<- bytes of metadata.")
              elems <- continuation.map { case (_,p) => p.items }
            } yield elems
          case (_,_) => Nil.point[F]
        }
      }
    } yield Packets(items)

    result
  }

  def deserialize[F[_]: Logger](array: Array[Byte])(implicit me: MonadError[F,Throwable]): F[Stream[F,Message]] = {

    val S = new StateTExtensions[F,Array[Byte]]
    import S._

    val logger = Logger[F]

    def deserializationDispatcher: DST[F,Message] =
      for {
        header  <- headerDecoder.decode[F]
        message <- header.Type match {
          case FmModelMsgType.SetEgressInfo => egressSocketInfo[F]
          case FmModelMsgType.Iosf          => iosf[F]
          case FmModelMsgType.Packet        => packet[F](header.Port)
          case FmModelMsgType.CommandQuit   => quit[F]
          case _                            => notSupported[F]
        }
      } yield message

    def loop(arr: Array[Byte], stream: Stream[F,Message]): F[Stream[F,Message]] = {
      lazy val recipt: DST[F,(Array[Byte],Message)] = for {
        msgBytes        <- getS
        header          <- headerDecoder.decode[F]
        _               <- liftS(logger.trace(s"Header received: $header"))
        (current, next) =  msgBytes.splitAt(header.Msglength)
        _               <- putS(current)
        msg             <- deserializationDispatcher
      } yield (next,msg)

      if (arr.isEmpty) {
        me.point(stream)
      } else {
        recipt.run(arr).flatMap {
          case (_,(nextArray,msg)) =>
            loop(nextArray, stream ++ Stream.emit(msg))
        }
      }
    }

    loop(array, Stream.empty)
  }

}
