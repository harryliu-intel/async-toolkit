package madisonbay

import cats.effect._
import cats.syntax.all._
import scalaz.MonadError
import madisonbay.logger.io.console._
import madisonbay.fs2app.Fs2Application._

object Main extends IOApp {

  implicit val catsIoMonadError: MonadError[IO,Throwable] = new MonadError[IO,Throwable] {
    def point[A](a: => A): IO[A] = IO.unit.map(_ => a)
    def bind[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)
    def handleError[A](fa: IO[A])(f: Throwable => IO[A]): IO[A] =
      fa.attempt.flatMap(_.fold(f, a => point(a)))
    def raiseError[A](e: Throwable): IO[A] = IO.raiseError(e)
  }

  def run(args: List[String]): IO[ExitCode] = program[IO].as(ExitCode.Success)
}
