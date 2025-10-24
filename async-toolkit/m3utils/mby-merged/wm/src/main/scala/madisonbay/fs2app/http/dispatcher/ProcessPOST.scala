package madisonbay.fs2app.http.dispatcher

import cats.effect.IO
import fs2.{RaiseThrowable, Stream}
import madisonbay.fs2app.http.dispatcher.ProcessRequest.ProcessRequestResult
import madisonbay.iface.model.CsrModel
import madisonbay.iface.rest.RestDispatcher
import madisonbay.wm.utils.json.JsonReader
import spinoco.fs2.http.HttpResponse
import spinoco.protocol.http.{HttpRequestHeader, HttpStatusCode, Uri}
import spinoco.protocol.mime.MediaType
import spinoco.protocol.http.header._
import spinoco.protocol.mime.ContentType.BinaryContent

import scala.util.{Failure, Success}


class ProcessPOST(request: HttpRequestHeader, requestBody: Stream[IO,Byte], csrModel: CsrModel)(implicit rt: RaiseThrowable[IO])
  extends ProcessRequest(request, requestBody, csrModel)(rt) {

  override def processRequest: ProcessRequestResult = {
    request.path match {

      case Uri.Path(false, _, _)  => mainHttpPage

      case Uri.Path(true, _, segments) if segments.isEmpty => mainHttpPage

      case Uri.Path(_, _, segments) =>
        val uri = segments.toList
        request.headers.collectFirst { case `Content-Type`(ct) => ct } match {

          case Some(BinaryContent(MediaType.`application/json`, _)) =>
            logger.info("Found json mediatype")
            processPostJson(uri)

          case Some(BinaryContent(MediaType.`application/octet-stream`, _)) =>
            emptyRequestResultOk

          case anyMediaType =>
            logger.info(s"Found mediatype X: $anyMediaType")
            emptyRequestResultOk
        }

    }
  }

  def processPostJson(uri: List[String]): ProcessRequestResult = {
    val body = getBody
    logger.info(s"Trying to parse input json $body")
    JsonReader.parse(body) match {

      case Success(jsonMap) =>
        processRestResponse(uri, RestDispatcher.processPostJson(uri, parameters, jsonMap, csrModel))

      case Failure(ex) => ProcessRequestResult(
        HttpResponse(HttpStatusCode.InternalServerError).
          withUtf8Body(s"""{ "msg": "Unable to parse input JSON", "reason": "${ex.getMessage}" }"""),
        None
      )

    }
  }

}
