package madisonbay.logger

import scalaz.{Monad,StateT}
import sourcecode.Line
import sourcecode.File

trait Logger[F[_]] {
  def debug(msg: => String)(implicit line: Line, file: File): F[Unit]
  def error(msg: => String)(implicit line: Line, file: File): F[Unit]
  def info(msg: => String)(implicit line: Line, file: File): F[Unit]
  def warn(msg: => String)(implicit line: Line, file: File): F[Unit]
  def trace(msg: => String)(implicit line: Line, file: File): F[Unit]
}

object Logger {
  implicit def stateTLogger[F[_]: Monad,S](implicit LF: Logger[F]): Logger[StateT[F,S,?]] = {
    import scalaz.syntax.all._

    type ST[A] = StateT[F,S,A]

    def log(l: => F[Unit]): ST[Unit] = StateT(s => l.map((s,_)))

    new Logger[StateT[F,S,?]] {
      def debug(msg: => String)(implicit line: Line, file: File): ST[Unit] = log(LF.debug(msg))
      def error(msg: => String)(implicit line: Line, file: File): ST[Unit] = log(LF.error(msg))
      def info(msg: => String)(implicit line: Line, file: File): ST[Unit] = log(LF.info(msg))
      def warn(msg: => String)(implicit line: Line, file: File): ST[Unit] = log(LF.warn(msg))
      def trace(msg: => String)(implicit line: Line, file: File): ST[Unit] = log(LF.trace(msg))
    }
  }

  def apply[F[_]](implicit logger: Logger[F]): Logger[F] = logger
}

