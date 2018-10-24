package madisonbay

import cats.effect._
import cats.syntax.all._
import scalaz.MonadError
import madisonbay.logger.io.console._
import madisonbay.fs2app.Fs2Application._

import java.nio.channels.AsynchronousChannelGroup
import java.nio.channels.spi.AsynchronousChannelProvider

import java.util.concurrent.Executors

object Main extends IOApp {

  val nThreads = 8

  implicit val catsIoMonadError: MonadError[IO,Throwable] = new MonadError[IO,Throwable] {
    def point[A](a: => A): IO[A] = IO.unit.map(_ => a)
    def bind[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)
    def handleError[A](fa: IO[A])(f: Throwable => IO[A]): IO[A] =
      fa.attempt.flatMap(_.fold(f, a => point(a)))
    def raiseError[A](e: Throwable): IO[A] = IO.raiseError(e)
  }

  implicit val acg: AsynchronousChannelGroup =
    AsynchronousChannelProvider
      .provider()
      .openAsynchronousChannelGroup(nThreads, Executors.defaultThreadFactory())

  implicit val ss = fs2ServerSocket[IO]
  implicit val ps = fs2PublisherSocket[IO]

  def run(args: List[String]): IO[ExitCode] = program[IO].as(ExitCode.Success)
}
