package madisonbay.fs2app.http

import cats.effect.IO
import fs2.{RaiseThrowable, Stream}
import spinoco.fs2.http.HttpResponse
import spinoco.protocol.http.{HttpRequestHeader, HttpStatusCode}
import spinoco.protocol.mime.{ContentType, MediaType}

object IoUriDispatcher extends UriDispatcher[IO] {

  override def processRequest(request: HttpRequestHeader, body: Stream[IO,Byte])(implicit rt: RaiseThrowable[IO]): HttpResponse[IO] = {

    val result = "<html><body>Hello World</body></html>"

    val contentType = ContentType.TextContent(MediaType.`text/html`, None)
    HttpResponse(HttpStatusCode.Ok).withUtf8Body(result).withContentType(contentType)
  }

}
