package madisonbay.iface.rest.services

import madisonbay.iface.UriConstants.UriParameter
import madisonbay.iface.model.CsrModel
import madisonbay.iface.rest.RestResponse
import madisonbay.iface.rest.services.RestProcessing._
import madisonbay.wm.utils.json.JsonReader
import spinoco.protocol.http.HttpStatusCode

import scala.util.{Failure, Success, Try}

object RestProcessing {

  val KeyMessage = "message"

  def responseMessage(msg: String): String = Try(JsonReader.toJson(Map[String, Any](
      KeyMessage -> msg
    ))) match {
      case Success(value) => value
      case Failure(ex)    => s"""{"$KeyMessage": $msg, "internal_error:": "${ex.getMessage}" }"""
    }

}

abstract class RestProcessing {

  def processGetRequest(uri: List[String], parameters: List[UriParameter], csrModel: CsrModel): RestResponse

  def uriPath(uri: List[String]): String = uri.mkString("/")

  def returnJson(result: Map[String, Any]): RestResponse = Try(JsonReader.toJson(result)) match {

    case Success(v)  => RestResponse(uriSupported = true, error = false, v, HttpStatusCode.Ok, None)

    case Failure(ex) => RestResponse(uriSupported = true, error = true, responseMessage(ex.getMessage), HttpStatusCode.InternalServerError, None)

  }

  def returnStdNotSupported(path: String): RestResponse = RestResponse(
    uriSupported = false,
    error = false,
    responseMessage(s"URI $path not supported"),
    HttpStatusCode.NotFound,
    None)

  def returnInternalServerError(msg: String): RestResponse = RestResponse(
    uriSupported = true,
    error = true,
    s"""{"$KeyMessage": "$msg"}""",
    HttpStatusCode.InternalServerError,
    None
  )

}
