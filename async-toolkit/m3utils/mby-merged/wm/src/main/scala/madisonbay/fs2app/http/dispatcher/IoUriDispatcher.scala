package madisonbay.fs2app.http.dispatcher

import cats.effect.IO
import fs2.{RaiseThrowable, Stream}
import madisonbay.iface.model.CsrModel
import madisonbay.fs2app.http.UriDispatcher
import madisonbay.wm.switchwm.csr.Csr
import spinoco.fs2.http.HttpResponse
import spinoco.protocol.http.{HttpMethod, HttpRequestHeader, HttpStatusCode}


object IoUriDispatcher extends UriDispatcher[IO] {

  val csrModel = new CsrModel(Csr(), limitNumberOfNodes = 32)

  override def processRequest(request: HttpRequestHeader, body: Stream[IO,Byte])
                             (implicit rt: RaiseThrowable[IO]): HttpResponse[IO] = {
    request.method match {

      case HttpMethod.GET => ProcessGET(csrModel).processGetRequest(request)

      case anyHttpMethod => HttpResponse(HttpStatusCode.MethodNotAllowed).
        withUtf8Body(s"http method $anyHttpMethod not supported")
    }

  }

}
