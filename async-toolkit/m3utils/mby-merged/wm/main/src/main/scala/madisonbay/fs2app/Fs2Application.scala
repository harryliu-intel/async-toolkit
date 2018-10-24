package madisonbay
package fs2app

import fs2._
import fs2.concurrent.{ Queue, SignallingRef }
import fs2.io.tcp._
import cats.effect.{ Concurrent, ConcurrentEffect }
import java.net.InetSocketAddress
import java.net.InetAddress

import madisonbay.tcp._
import madisonbay.tcp.iosf._

import madisonbay.logger.Logger
import madisonbay.fs2app.algebra.messages._
import madisonbay.fs2app.deserialization._
import madisonbay.fs2app.algebra._

import scalaz.MonadError
import scalaz.syntax.all._

import java.nio.channels.AsynchronousChannelGroup

import com.intel.cg.hpfd.madisonbay.PrimitiveTypes._

object Fs2Application {

  def fs2ServerSocket[F[_]: Logger: ConcurrentEffect: Concurrent]
    (implicit acg: AsynchronousChannelGroup): ServerSocket[F] =
    new ServerSocket[F] {
      def create: Stream[F,Socket[F]] = {
        val CF = Concurrent[F]
        val logger = Logger[F]

        val serverPort = 5888
        val serverHostname = InetAddress.getLocalHost().getHostName()
        val isa = new InetSocketAddress(serverHostname, serverPort)

        for {
          _        <- Stream.eval(logger.info(s"Starting server..."))
          _        <- Stream.bracket(CF.delay(acg))(asyncCg => CF.delay(asyncCg.shutdown))
          resource <- server[F](bind = isa)
          _        <- Stream.eval(logger.info(s"Server started at $serverHostname:$serverPort"))
          socket   <- Stream.resource(resource)
        } yield socket
      }
    }

  def fs2PublisherSocket[F[_]: ConcurrentEffect: Concurrent]
    (implicit acg: AsynchronousChannelGroup): PublisherSocket[F] =
    new PublisherSocket[F] {
      def create(egressSocketInfo: EgressSocketInfo): Stream[F,Socket[F]] = {
        val EgressSocketInfo(hostname, port) = egressSocketInfo
        val clientIsa = new InetSocketAddress(InetAddress.getByName(hostname), port)
        Stream.resource(client[F](to = clientIsa, noDelay = true))
      }
    }

  private class Application[F[_]:
      Logger:
      ConcurrentEffect:
      Concurrent:
      ServerSocket:
      PublisherSocket
  ](egressSocketInfoQ: Queue[F,EgressSocketInfo], quitS: SignallingRef[F,Boolean])
    (implicit me: MonadError[F,Throwable]) {

    val logger = Logger[F]
    val serverSocket = ServerSocket[F]
    val publisherSocket = PublisherSocket[F]

    //scalastyle:off method.length
    def handleMessage(msg: Message): Stream[F,Array[Byte]] = msg match {
      case esi @ EgressSocketInfo(_,_) =>
        val esiS = Stream.eval(esi.point[F])
        for {
          info <- esiS
          _    <- Stream.eval(logger.trace(s"Queuing egress socket info: $info"))
          _    <- egressSocketInfoQ.enqueue(esiS)
        } yield Array.empty[Byte]
      case io @ IosfRegBlkWrite(req,_) => Stream.eval(
        for {
          _ <- logger.debug(s"Received IosfRegBlkWrite: $io")
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
        } yield ByteArrayEncoder[FmModelMessageHdr].encode(header) ++
          ByteArrayEncoder[IosfRegCompNoData].encode(iosfNoData)
      )
      case io @ (IosfRegRead(req)) => Stream.eval(
        for {
          _ <- logger.debug(s"Received $io")
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
          _ <- logger.debug(s"Sending back hardcoded register value: $registerValue")
        } yield ByteArrayEncoder[FmModelMessageHdr].encode(header) ++
          ByteArrayEncoder[IosfRegCompDataHdr].encode(iosfData) ++
          ByteArrayEncoder[U64].encode(registerValue)
      )
      case Quit => Stream.eval(
        for {
          _ <- logger.debug("Terminating program! Bye bye!")
          _ <- quitS.set(true)
        } yield Array.empty[Byte]
      )
      case _ => handleMessage(Quit)
    }

    def serverStream: Stream[F,Stream[F,Unit]] =
      for {
        socket     <- serverSocket.create
        bufferSize  = 16384 //scalastyle:ignore magic.number
      } yield socket.reads(bufferSize)
        .chunks
        .map(_.toArray)
        .map(deserialize[F])
        .evalMap(identity)
        .flatMap(identity)
        .flatMap(handleMessage)
        .map(array => socket.write(Chunk.boxed(array)))
        .evalMap(identity)

    def publisherStream: Stream[F,Stream[F,Unit]] =
      egressSocketInfoQ.dequeue
        .flatMap(publisherSocket.create)
        .map(socket => Stream.eval(socket.close))
  }

  def program[F[_]:
      Logger:
      Concurrent:
      ConcurrentEffect:
      ServerSocket:
      PublisherSocket
  ](implicit me: MonadError[F,Throwable]): F[Unit] = {
    val queueSize = 1

    val stream = for {
      egressQ <- Stream.eval(Queue.bounded[F,EgressSocketInfo](queueSize))
      quitS   <- Stream.eval(SignallingRef[F,Boolean](false))
      app     =  new Application[F](egressQ, quitS)
      par     =  10 //scalastyle:ignore magic.number
      _       <- {
        import app._
        (serverStream concurrently publisherStream)
          .interruptWhen(quitS)
          .parJoin(par)
      }
    } yield ()

    stream.compile.drain
  }

}
