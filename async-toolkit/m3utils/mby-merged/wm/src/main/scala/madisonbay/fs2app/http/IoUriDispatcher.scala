package madisonbay.fs2app.http

import cats.effect.IO
import fs2.{RaiseThrowable, Stream}
import spinoco.fs2.http.HttpResponse
import spinoco.protocol.http._
import spinoco.protocol.mime.{ContentType, MediaType}
import madisonbay.frontend.{PageGenerator, ResourceProvider}


object IoUriDispatcher extends UriDispatcher[IO] {

  case class UriParameter(key: String, value: Option[String])

  val contentFrontend   = ContentType.TextContent(MediaType.`text/html`, None)
  val contentCss        = ContentType.BinaryContent(MediaType.`text/css`, None)
  val contentImgPng     = ContentType.BinaryContent(MediaType.`image/png`, None)

  val CssUriPref = "css"
  val ImgUriPref = "img"

  override def processRequest(request: HttpRequestHeader, body: Stream[IO,Byte])
                             (implicit rt: RaiseThrowable[IO]): HttpResponse[IO] = {
    val parameters = getParameters(request.query)

    request.path match {

      case Uri.Path(false, _, _)      =>
        HttpResponse(HttpStatusCode.Ok).withUtf8Body(PageGenerator.mainPage(parameters)).withContentType(contentFrontend)

      case Uri.Path(true, _, segments) if segments.isEmpty   =>
        HttpResponse(HttpStatusCode.Ok).withUtf8Body(PageGenerator.mainPage(parameters)).withContentType(contentFrontend)

      case Uri.Path(_, _, segments)   => processGet(segments.toList, parameters)

    }

  }

  private def getParameters(query: Uri.Query): List[UriParameter] = query.collect[UriParameter] {
    case Uri.QueryParameter.Single(k, v)  => UriParameter(k, Some(v))
    case Uri.QueryParameter.Flag(f)       => UriParameter(f, None)
  }

  private def processGet(uri: List[String], parameters: List[UriParameter])
                        (implicit rt: RaiseThrowable[IO]): HttpResponse[IO] = uri match {
    case CssUriPref :: ImgUriPref :: tail => processGet(ImgUriPref :: tail, parameters)

    case CssUriPref :: fileName :: Nil =>
      val cssBody = ResourceProvider.getCss(fileName)
      HttpResponse(HttpStatusCode.Ok).withUtf8Body(cssBody).withContentType(contentCss)

    case ImgUriPref :: fileName :: Nil if fileName.endsWith(".png") =>
      val imgBody: List[Byte] = ResourceProvider.getImg(fileName)
      val eff = Stream[IO,Byte](imgBody: _*)
      val ok = HttpResponse(HttpStatusCode.Ok).withContentType(contentImgPng)
      HttpResponse(ok.header, eff)

      // URI not supported
    case u =>
      HttpResponse(HttpStatusCode.Ok).
        withUtf8Body(PageGenerator.processGet(" [Error] Not Supported URI: " :: u, parameters)).withContentType(contentFrontend)
  }

}
