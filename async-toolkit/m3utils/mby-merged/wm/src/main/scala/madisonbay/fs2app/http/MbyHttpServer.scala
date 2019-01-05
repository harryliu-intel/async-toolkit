package madisonbay.fs2app.http

import fs2.Stream

trait MbyHttpServer[F[_]] {
  def create: Stream[F, Stream[F, Unit]]
}

object MbyHttpServer {
  def apply[F[_]](implicit hp: MbyHttpServer[F]): MbyHttpServer[F] = hp
}
