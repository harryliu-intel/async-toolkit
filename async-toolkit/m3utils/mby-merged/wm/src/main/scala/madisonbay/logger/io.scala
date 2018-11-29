package madisonbay
package logger

import cats.effect.IO
import com.typesafe.scalalogging.LazyLogging
import sourcecode.{ File, Line }

package object io {

  @inline
  private def format(msg: => String)(implicit line: Line, file: File): String =
    s"${file.value}:${line.value} - $msg"

  object console { //scalastyle:ignore object.name
    implicit val ioLogger = new Logger[IO] {
      def debug(msg: => String)(implicit line: Line, file: File): IO[Unit] =
        IO.delay(println(format(msg))) //scalastyle:ignore noprntln
      def error(msg: => String)(implicit line: Line, file: File): IO[Unit] = debug(msg)
      def info(msg: => String)(implicit line: Line, file: File): IO[Unit] = debug(msg)
      def warn(msg: => String)(implicit line: Line, file: File): IO[Unit] = debug(msg)
      def trace(msg: => String)(implicit line: Line, file: File): IO[Unit] = debug(msg)
    }
  }

  object logback { //scalastyle:ignore object.name
    implicit val ioLogger = new Logger[IO] with LazyLogging {
      def debug(msg: => String)(implicit line: Line, file: File): IO[Unit] =
        IO.delay(logger.debug(format(msg)))
      def error(msg: => String)(implicit line: Line, file: File): IO[Unit] =
        IO.delay(logger.error(format(msg)))
      def info(msg: => String)(implicit line: Line, file: File): IO[Unit] =
        IO.delay(logger.info(format(msg)))
      def warn(msg: => String)(implicit line: Line, file: File): IO[Unit] =
        IO.delay(logger.warn(format(msg)))
      def trace(msg: => String)(implicit line: Line, file: File): IO[Unit] =
        IO.delay(logger.trace(format(msg)))
    }
  }

}
