package madisonbay
package fs2app

import fs2.Sink
import madisonbay.fs2app.algebra.{PublisherSocket, ServerSocket}
import madisonbay.fs2app.algebra.messages._
import madisonbay.fs2app.algebra.PublisherSocket
import madisonbay.logger.Logger

import fs2.io.tcp.Socket
import fs2.{ Chunk, Sink, Stream }
import cats.effect.IO
import scalaz.MonadError

import org.scalatest.Matchers
import org.scalatest.compatible.Assertion

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

abstract class IOTestContext[Root] extends Matchers {

  def payload: Array[Byte] = Array.empty
  def expectedResponseSink: Sink[IO,Byte] = _.map(_ => ())
  def publisherSocketCallback(esi: EgressSocketInfo): Assertion = {
    val _ = esi
    true shouldEqual true
  }

  implicit val emptyLogger: Logger[IO] = new Logger[IO] {
    def debug(msg: => String): IO[Unit] = IO.unit
    def error(msg: => String): IO[Unit] = IO.unit
    def info(msg: => String): IO[Unit] = IO.unit
    def warn(msg: => String): IO[Unit] = IO.unit
    def trace(msg: => String): IO[Unit] = IO.unit
  }

  implicit lazy val serverSocket = new ServerSocket[IO] {
    def create(): Stream[IO,Socket[IO]] = Stream.emit(
      new Socket[IO] {
        def close: IO[Unit] = IO.unit
        def endOfInput: IO[Unit] = IO.unit
        def endOfOutput: IO[Unit] = IO.unit
        def localAddress: IO[java.net.SocketAddress] = ???
        def read(maxBytes: Int,timeout: Option[FiniteDuration]): IO[Option[Chunk[Byte]]] = ???
        def readN(numBytes: Int,timeout: Option[FiniteDuration]): IO[Option[Chunk[Byte]]] = ???
        def reads(maxBytes: Int,timeout: Option[FiniteDuration]): Stream[IO,Byte] = Stream(payload: _*)
        def remoteAddress: IO[java.net.SocketAddress] = ???
        def write(bytes: Chunk[Byte],timeout: Option[FiniteDuration]): IO[Unit] = ???
        def writes(timeout: Option[FiniteDuration]): Sink[IO,Byte] = expectedResponseSink
      }
    )
  }

  implicit lazy val ps = new PublisherSocket[IO] {
    def create(esi: EgressSocketInfo): Stream[IO,Socket[IO]] = {
      publisherSocketCallback(esi)
      Stream.emit(
        new Socket[IO] {
          def close: IO[Unit] = IO.unit
          def endOfInput: IO[Unit] = IO.unit
          def endOfOutput: IO[Unit] = IO.unit
          def localAddress: IO[java.net.SocketAddress] = ???
          def read(maxBytes: Int,timeout: Option[FiniteDuration]): IO[Option[Chunk[Byte]]] = ???
          def readN(numBytes: Int,timeout: Option[FiniteDuration]): IO[Option[Chunk[Byte]]] = ???
          def reads(maxBytes: Int,timeout: Option[FiniteDuration]): Stream[IO,Byte] = Stream.empty
          def remoteAddress: IO[java.net.SocketAddress] = ???
          def write(bytes: Chunk[Byte],timeout: Option[FiniteDuration]): IO[Unit] = IO.unit
          def writes(timeout: Option[FiniteDuration]): Sink[IO,Byte] = ???
        }
      )
    }
  }

  implicit val catsIoMonadError: MonadError[IO,Throwable] = new MonadError[IO,Throwable] {
    def point[A](a: => A): IO[A] = IO.unit.map(_ => a)
    def bind[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)
    def handleError[A](fa: IO[A])(f: Throwable => IO[A]): IO[A] =
      fa.attempt.flatMap(_.fold(f, a => point(a)))
    def raiseError[A](e: Throwable): IO[A] = IO.raiseError(e)
  }
  implicit val mh = new Fs2DefaultMessageHandler[IO, Root]
  implicit val cs = IO.contextShift(ExecutionContext.global)
}
