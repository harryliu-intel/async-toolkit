package madisonbay.fs2app.http.dispatcher

import cats.effect.IO
import com.typesafe.scalalogging.LazyLogging
import fs2.{RaiseThrowable, Stream}
import madisonbay.fs2app.http.dispatcher.ProcessRequest.{ProcessRequestResult, contentHtml, contentJson}
import madisonbay.iface.UriConstants.UriParameter
import madisonbay.iface.frontend.viewer.PageGenerator
import madisonbay.iface.model.CsrModel
import madisonbay.iface.rest.RestResponse
import spinoco.fs2.http.HttpResponse
import spinoco.protocol.http.header.`Content-Length`
import spinoco.protocol.http.{HttpRequestHeader, HttpStatusCode, Uri}
import spinoco.protocol.mime.{ContentType, MediaType}

object ProcessRequest {
  val contentHtml       = ContentType.TextContent(MediaType.`text/html`, None)
  val contentCss        = ContentType.BinaryContent(MediaType.`text/css`, None)
  val contentImgPng     = ContentType.BinaryContent(MediaType.`image/png`, None)
  val contentJson       = ContentType.TextContent(MediaType.`application/json`, None)

  case class ProcessRequestResult(httpResponse: HttpResponse[IO], csrModelOpt: Option[CsrModel])
}


abstract class ProcessRequest(request: HttpRequestHeader, requestBody: Stream[IO,Byte], csrModel: CsrModel)
                             (implicit rt: RaiseThrowable[IO]) extends LazyLogging {

  val parameters: List[UriParameter] = getParameters(request.query)

  val contentLength: Long = request.headers.collectFirst { case `Content-Length`(sz) => sz }.getOrElse(0L)

  def processRequest: ProcessRequestResult

  def getParameters(query: Uri.Query): List[UriParameter] = query.collect[UriParameter] {
    case Uri.QueryParameter.Single(k, v)  => UriParameter(k, Some(v))
    case Uri.QueryParameter.Flag(f)       => UriParameter(f, None)
  }

  def getBody: String = {
    val body: List[Byte] = requestBody.take(contentLength).compile.toList.unsafeRunSync()
    body.map(_.toChar).mkString
  }

  def mainHttpPage: ProcessRequestResult =
    ProcessRequestResult(
      HttpResponse(HttpStatusCode.Ok).
        withUtf8Body(PageGenerator.mainPage(parameters, csrModel)).
        withContentType(contentHtml),
      None)

  def processRestResponse(uri: List[String], restResponse: RestResponse): ProcessRequestResult = restResponse match {

      case RestResponse(false, _, _, httpStatusCode, _) =>
        ProcessRequestResult(
          HttpResponse(httpStatusCode).
            withUtf8Body(PageGenerator.genPage(" [Error] Not Supported URI: " :: uri, parameters, csrModel)).
            withContentType(contentHtml),
          None)

      case RestResponse(_, _, "", httpStatusCode, csrModelOpt) => ProcessRequestResult(HttpResponse(httpStatusCode), csrModelOpt)

      case RestResponse(_, _, msg, httpStatusCode, csrModelOpt) =>
        ProcessRequestResult(
          HttpResponse(httpStatusCode).withUtf8Body(msg).withContentType(contentJson),
          csrModelOpt
        )
  }

  def emptyRequestResultOk: ProcessRequestResult = ProcessRequestResult(HttpResponse(HttpStatusCode.Ok), None)

}
