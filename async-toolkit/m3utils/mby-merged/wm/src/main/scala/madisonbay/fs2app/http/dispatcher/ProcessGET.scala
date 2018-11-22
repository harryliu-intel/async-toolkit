package madisonbay.fs2app.http.dispatcher

import cats.effect.IO
import fs2.{RaiseThrowable, Stream}
import madisonbay.iface.frontend.controller.ResourceProvider
import madisonbay.iface.frontend.model.CsrModel
import madisonbay.iface.frontend.viewer.PageGenerator
import madisonbay.iface.frontend.viewer.UriConstants.UriParameter
import madisonbay.iface.frontend.viewer.UriConstants.UriSegment.{CssUriPref, ImgUriPref}
import spinoco.fs2.http.HttpResponse
import spinoco.protocol.http.{HttpRequestHeader, HttpStatusCode, Uri}
import spinoco.protocol.mime.{ContentType, MediaType}
import ProcessGET._


object ProcessGET {
  val contentFrontend   = ContentType.TextContent(MediaType.`text/html`, None)
  val contentCss        = ContentType.BinaryContent(MediaType.`text/css`, None)
  val contentImgPng     = ContentType.BinaryContent(MediaType.`image/png`, None)

  def apply(csrModel: CsrModel): ProcessGET = new ProcessGET(csrModel)
}

class ProcessGET(csrModel: CsrModel) extends ProcessRequest {

  def processGetRequest(request: HttpRequestHeader)(implicit rt: RaiseThrowable[IO]): HttpResponse[IO] = {
    val parameters = getParameters(request.query)

    def httpPage = {
      val page = PageGenerator.mainPage(parameters, csrModel)
      HttpResponse(HttpStatusCode.Ok).withUtf8Body(page).withContentType(contentFrontend)
    }

    request.path match {

      case Uri.Path(false, _, _)  => httpPage

      case Uri.Path(true, _, segments) if segments.isEmpty => httpPage

      case Uri.Path(_, _, segments) => processGet(segments.toList, parameters)

    }
  }

  private def processGet(uri: List[String], parameters: List[UriParameter])
                        (implicit rt: RaiseThrowable[IO]): HttpResponse[IO] = uri match {
    case CssUriPref :: ImgUriPref :: tail => processGet(ImgUriPref :: tail, parameters)

    case CssUriPref :: fileName :: Nil =>
      val cssBody = ResourceProvider.getCss(fileName)
      HttpResponse(HttpStatusCode.Ok).withUtf8Body(cssBody).withContentType(contentCss)

    case ImgUriPref :: fileName :: Nil if fileName.endsWith(".png") =>
      val imgBody: List[Byte] = ResourceProvider.getImg(fileName)
      val effectBody = Stream[IO,Byte](imgBody: _*)
      val responseOk = HttpResponse(HttpStatusCode.Ok)
      HttpResponse(responseOk.header, effectBody).withContentType(contentImgPng)

    // URI not supported
    case u =>
      HttpResponse(HttpStatusCode.Ok).
        withUtf8Body(PageGenerator.processGet(" [Error] Not Supported URI: " :: u, parameters, csrModel)).
        withContentType(contentFrontend)
  }


}
