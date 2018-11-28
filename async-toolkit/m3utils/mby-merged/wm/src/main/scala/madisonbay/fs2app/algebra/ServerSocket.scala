package madisonbay
package fs2app
package algebra

import fs2.Stream
import fs2.io.tcp.Socket

trait ServerSocket[F[_]] {
  def create: Stream[F,Socket[F]]
}

object ServerSocket {
  def apply[F[_]](implicit ss: ServerSocket[F]): ServerSocket[F] = ss
}
