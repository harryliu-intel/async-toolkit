package madisonbay.fs2app.http

import fs2.Stream

trait HttpServer[F[_]] {
  def create: Stream[F, Stream[F, Unit]]
}

object HttpServer {
  def apply[F[_]](implicit hp: HttpServer[F]): HttpServer[F] = hp
}
