package madisonbay.logger

trait Logger[F[_]] {
  def debug(msg: => String): F[Unit]
  def error(msg: => String): F[Unit]
  def info(msg: => String): F[Unit]
  def warn(msg: => String): F[Unit]
  def trace(msg: => String): F[Unit]
}

object Logger {
  def apply[F[_]](implicit logger: Logger[F]): Logger[F] = logger
}

