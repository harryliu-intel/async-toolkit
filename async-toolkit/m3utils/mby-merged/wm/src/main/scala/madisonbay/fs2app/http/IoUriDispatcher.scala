package madisonbay.fs2app.http

import cats.effect.IO
import fs2.{RaiseThrowable, Stream}
import spinoco.fs2.http.HttpResponse
import spinoco.protocol.http._
import spinoco.protocol.mime.{ContentType, MediaType}

import madisonbay.frontend.PageGenerator

object IoUriDispatcher extends UriDispatcher[IO] {

  case class UriParameter(key: String, value: Option[String])

  val contentFrontend = ContentType.TextContent(MediaType.`text/html`, None)

  override def processRequest(request: HttpRequestHeader, body: Stream[IO,Byte])(implicit rt: RaiseThrowable[IO]): HttpResponse[IO] = {
    val parameters = getParameters(request.query)

    val (htmlBody, contentType) = request.path match {
      case Uri.Path(false, _, _)      => (PageGenerator.mainPage(parameters), contentFrontend)
      case Uri.Path(true, _, segments) if segments.isEmpty   => (PageGenerator.mainPage(parameters), contentFrontend)
      case Uri.Path(_, _, segments)   => processUri(segments.toList, parameters)
    }

    HttpResponse(HttpStatusCode.Ok).withUtf8Body(htmlBody).withContentType(contentType)
  }

  def processUri(uri: List[String], parameters: List[UriParameter]): (String, ContentType) = uri match {
    case _ => (PageGenerator.processGet(uri, parameters), contentFrontend)
  }

  private def getParameters(query: Uri.Query): List[UriParameter] = query.collect[UriParameter] {
    case Uri.QueryParameter.Single(k, v)  => UriParameter(k, Some(v))
    case Uri.QueryParameter.Flag(f)       => UriParameter(f, None)
  }

}
