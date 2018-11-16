package madisonbay.fs2app.http

import fs2.{RaiseThrowable, Stream}
import spinoco.fs2.http.HttpResponse
import spinoco.protocol.http._

trait UriDispatcher[F[_]] {
  def processRequest(request: HttpRequestHeader, body: Stream[F,Byte])(implicit rt: RaiseThrowable[F]): HttpResponse[F]
}

object UriDispatcher {
  def apply[F[_]](implicit ud: UriDispatcher[F]): UriDispatcher[F] = ud
}
