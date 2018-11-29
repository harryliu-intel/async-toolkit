package madisonbay.fs2app.http.dispatcher

import cats.effect.IO
import com.typesafe.scalalogging.LazyLogging
import fs2.{RaiseThrowable, Stream}
import madisonbay.iface.model.CsrModel
import madisonbay.fs2app.http.UriDispatcher
import madisonbay.fs2app.http.dispatcher.ProcessRequest.ProcessRequestResult
import madisonbay.wm.switchwm.csr.Csr
import spinoco.fs2.http.HttpResponse
import spinoco.protocol.http.{HttpMethod, HttpRequestHeader, HttpStatusCode}



object IoUriDispatcher extends UriDispatcher[IO] with LazyLogging {

  val LimitNumberOfNodes = 64

  // TODO: use state from fs2app
  var csrModel = new CsrModel(Csr(), limitNumberOfNodes = LimitNumberOfNodes)

  override def processRequest(request: HttpRequestHeader, body: Stream[IO,Byte])
                             (implicit rt: RaiseThrowable[IO]): HttpResponse[IO] = {
    request.method match {

      case HttpMethod.GET => processResponse(new ProcessGET(request, body, csrModel).processRequest)

      case HttpMethod.POST => processResponse(new ProcessPOST(request, body, csrModel).processRequest)

      case anyHttpMethod => HttpResponse(HttpStatusCode.MethodNotAllowed).
        withUtf8Body(s"http method $anyHttpMethod not supported")
    }

  }

  def processResponse(processRequestResult: ProcessRequestResult): HttpResponse[IO] = {
    if (processRequestResult.csrModelOpt.nonEmpty) {
      csrModel = processRequestResult.csrModelOpt.get
    }
    processRequestResult.httpResponse
  }

}

