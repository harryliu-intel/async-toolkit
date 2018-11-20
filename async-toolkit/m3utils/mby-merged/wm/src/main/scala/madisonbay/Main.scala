package madisonbay

import cats.effect._
import cats.syntax.all._
import scalaz.MonadError
import monocle.Optional
import madisonbay.csr._
import madisonbay.csr.all._
import madisonbay.logger.Logger
import madisonbay.logger.io.logback._
import madisonbay.fs2app.algebra._
import madisonbay.fs2app.algebra.messages._
import madisonbay.config.Config
import madisonbay.fs2app.Fs2Application._
import madisonbay.fs2app.Fs2DefaultMessageHandler
import madisonbay.fs2app.http._
import madisonbay.fs2app.ioConfig.IOPureConfigLoader
import com.intel.cg.hpfd.madisonbay.Memory._
import java.nio.channels.AsynchronousChannelGroup
import java.nio.channels.spi.AsynchronousChannelProvider
import java.util.concurrent.Executors

import madisonbay.fs2app.http.MbyHttpServer


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

  implicit val conf     = IOPureConfigLoader.load()
  implicit val ss       = fs2ServerSocket[IO]
  implicit val ps       = fs2PublisherSocket[IO]
  implicit val handler  = new Fs2DefaultMessageHandler[IO,mby_top_map]
  implicit val ud       = IoUriDispatcher
  implicit val hp       = fs2HttpStream[IO]

  def program[F[_]:
      Logger:
      Config:
      Concurrent:
      ConcurrentEffect:
      ServerSocket:
      PublisherSocket:
      MbyHttpServer:
      λ[G[_] => MonadError[G,Throwable]]:
      λ[G[_] => MessageHandler[G,CsrContext[mby_top_map]]]
  ]: F[Unit] = {
    val logger = Logger[F]
    val config = Config[F]
    for {
      firstByte <- config.int("initialAddressInBytes")
      address    = Address at firstByte.bytes
      _         <- logger.info(s"Creating registers, starting from $address...")
      root       = mby_top_map(address)
      _         <- logger.info("[DONE]")
      _         <- logger.info("Creating registers optics...")
      paths      = mby_top_map.genOpticsLookup(root, Optional.id)
      _         <- logger.info("[DONE]")
      _         <- fs2Program[CsrContext[mby_top_map],F](CsrContext(root, paths))
    } yield ()
  }

  def run(args: List[String]): IO[ExitCode] = program[IO].as(ExitCode.Success)
}
