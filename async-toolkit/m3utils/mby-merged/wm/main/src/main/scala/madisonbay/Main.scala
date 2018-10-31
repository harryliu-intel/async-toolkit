package madisonbay

import cats.effect._
import cats.syntax.all._
import scalaz.MonadError
import monocle.Optional

import madisonbay.csr._
import madisonbay.csr.all._
import madisonbay.logger.Logger
import madisonbay.logger.io.console._
import madisonbay.fs2app.algebra._
import madisonbay.fs2app.algebra.messages._
import madisonbay.fs2app.Fs2Application._
import madisonbay.fs2app.Fs2DefaultMessageHandler

import com.intel.cg.hpfd.madisonbay.Memory._

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
  implicit val handler = new Fs2DefaultMessageHandler[IO,mby_top_map]

  def program[F[_]:
      Logger:
      Concurrent:
      ConcurrentEffect:
      ServerSocket:
      PublisherSocket:
      λ[G[_] => MonadError[G,Throwable]]:
      λ[G[_] => MessageHandler[G,CsrContext[mby_top_map]]]
  ]: F[Unit] = {
    val logger = Logger[F]
    for {
      _      <- logger.trace("Creating registers...")
      address = Address at 0.bytes
      root    = mby_top_map(address)
      _      <- logger.trace("[DONE]")
      _      <- logger.trace("Creating registers optics...")
      paths   = mby_top_map.genOpticsLookup(root, Optional.id)
      _      <- logger.trace("[DONE]")
      _      <- fs2Program[CsrContext[mby_top_map],F](CsrContext(root, paths))
    } yield ()
  }

  def run(args: List[String]): IO[ExitCode] = program[IO].as(ExitCode.Success)
}
