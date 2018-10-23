package madisonbay
package logger

import cats.effect.IO

package object io {

  object console { //scalastyle:ignore object.name
    implicit val ioLogger = new Logger[IO] {
      def debug(msg: => String): IO[Unit] = IO.delay(println(msg)) //scalastyle:ignore noprntln
      def error(msg: => String): IO[Unit] = debug(msg)
      def info(msg: => String): IO[Unit] = debug(msg)
      def warn(msg: => String): IO[Unit] = debug(msg)
      def trace(msg: => String): IO[Unit] = debug(msg)
    }
  }

}
