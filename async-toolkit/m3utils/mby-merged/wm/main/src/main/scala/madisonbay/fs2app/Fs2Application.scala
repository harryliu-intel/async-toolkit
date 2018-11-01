package madisonbay
package fs2app

import fs2._
import fs2.concurrent.Queue
import fs2.io.tcp._
import cats.effect.{ Concurrent, ConcurrentEffect }
import java.net.InetSocketAddress
import java.net.InetAddress
import java.nio.channels.AsynchronousChannelGroup
import java.nio.channels.spi.AsynchronousChannelProvider

import java.util.concurrent.Executors

import madisonbay.logger.Logger
import madisonbay.messages._
import madisonbay.fs2app.deserialization._

import scalaz.MonadError
import scalaz.syntax.all._

object Fs2Application {

  private class Application[F[_]: Logger: ConcurrentEffect: Concurrent]
    (queue: Queue[F,Message])(implicit me: MonadError[F,Throwable]) {

    val logger = Logger[F]
    val nThreads = 8
    val serverPort = 5888
    val serverHostname = InetAddress.getLocalHost().getHostName()
    val isa = new InetSocketAddress(serverHostname, serverPort)
    implicit val acg: AsynchronousChannelGroup =
      AsynchronousChannelProvider
        .provider()
        .openAsynchronousChannelGroup(nThreads, Executors.defaultThreadFactory())

    def serverStream: Stream[F, Stream[F,Unit]] =
      for {
        _          <- Stream.eval(logger.info(s"Starting server: $serverHostname:$serverPort"))
        resource   <- server[F](bind = isa)
        socket     <- Stream.resource(resource)
        bufferSize =  16384 //scalastyle:ignore magic.number
      } yield
          socket.reads(bufferSize)
            .chunks
            .map(_.toArray)
            .map(deserialize[F])
            .flatMap(Stream.eval)
            .flatMap(identity)
            .flatMap {
              case esi @ EgressSocketInfo(_,_) =>
                val item = for {
                  info <- esi.point[F]
                  _    <- logger.debug(s"Queuing egress socket info: $info")
                } yield info
                queue.enqueue(Stream.eval(item))
              case io @ IosfRegBlkWrite(_) => Stream.eval(logger.debug(io.toString))
              case io @ IosfRegRead(_) => Stream.eval(logger.debug(io.toString))
              case io @ IosfRegWrite(_) => Stream.eval(logger.debug(io.toString))
              case any => Stream.eval(logger.debug(s":( received any: $any"))
            }

    def publisherStream: Stream[F,Stream[F,Unit]] =
      for {
        cp <- queue.dequeue
        EgressSocketInfo(hostname, port) = cp
        clientIsa = new InetSocketAddress(InetAddress.getByName(hostname), port)
        socket <- Stream.resource(client[F](to = clientIsa, noDelay = true))
      } yield Stream.eval(socket.close)
  }

  def program[F[_]: Logger: Concurrent: ConcurrentEffect](implicit me: MonadError[F,Throwable]): F[Unit] = {
    val queueSize = 1

    val stream = for {
      queue     <- Stream.eval(Queue.bounded[F,Message](queueSize))
      app       =  new Application[F](queue)
      par       =  10 //scalastyle:ignore magic.number
      _         <- app.serverStream.parJoin(par) merge app.publisherStream.parJoin(par)
    } yield ()

    stream.compile.drain
  }

}
