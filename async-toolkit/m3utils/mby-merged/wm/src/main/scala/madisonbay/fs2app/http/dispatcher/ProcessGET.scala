package madisonbay.fs2app.http.dispatcher

import cats.effect.IO
import fs2.{RaiseThrowable, Stream}
import madisonbay.iface.frontend.controller.ResourceProvider
import madisonbay.iface.model.CsrModel
import madisonbay.iface.UriConstants.UriSegment
import spinoco.fs2.http.HttpResponse
import spinoco.protocol.http.{HttpRequestHeader, HttpStatusCode, Uri}
import ProcessRequest._
import madisonbay.iface.rest.RestDispatcher



class ProcessGET(request: HttpRequestHeader, requestBody: Stream[IO,Byte], csrModel: CsrModel)(implicit rt: RaiseThrowable[IO])
  extends ProcessRequest(request, requestBody, csrModel)(rt) {

  override def processRequest: ProcessRequestResult = {
    request.path match {

      case Uri.Path(false, _, _)  => mainHttpPage

      case Uri.Path(true, _, segments) if segments.isEmpty => mainHttpPage

      case Uri.Path(true, _, segments) => processGetUri(segments.toList)

    }
  }

  private def processGetUri(uri: List[String]): ProcessRequestResult = uri match {
    case UriSegment.Css :: UriSegment.Img :: tail => processGetUri(UriSegment.Img :: tail)

    case UriSegment.Css :: fileName :: Nil =>
      val cssBody = ResourceProvider.getCss(fileName)
      ProcessRequestResult(HttpResponse(HttpStatusCode.Ok).withUtf8Body(cssBody).withContentType(contentCss), None)


    case UriSegment.Img :: fileName :: Nil if fileName.endsWith(".png") =>
      val imgBody: List[Byte] = ResourceProvider.getImg(fileName)
      val effectBody = Stream[IO,Byte](imgBody: _*)
      val responseOk = HttpResponse(HttpStatusCode.Ok)
      ProcessRequestResult(HttpResponse(responseOk.header, effectBody).withContentType(contentImgPng), None)

    case _ => processRestResponse(uri, RestDispatcher.processGetUri(uri, parameters, csrModel))

  }

}
