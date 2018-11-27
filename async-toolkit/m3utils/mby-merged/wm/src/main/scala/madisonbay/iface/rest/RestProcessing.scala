package madisonbay.iface.rest

import madisonbay.iface.UriConstants.UriParameter
import madisonbay.iface.model.CsrModel
import madisonbay.wm.utils.json.JsonReader
import spinoco.protocol.http.HttpStatusCode
import RestProcessing._

import scala.util.{Failure, Success, Try}

object RestProcessing {

  val KeyMessage = "message"

}

abstract class RestProcessing {

  def processGetRequest(uri: List[String], parameters: List[UriParameter], csrModel: CsrModel): RestResponse

  def uriPath(uri: List[String]): String = uri.mkString("/")

  def returnJson(result: Map[String, Any]): RestResponse = Try(JsonReader.toJson(result)) match {

    case Success(v) => RestResponse(uriSupported = true, error = false, v, HttpStatusCode.Ok)

    case Failure(ex) => RestResponse(uriSupported = true, error = true, responseMessage(ex.getMessage), HttpStatusCode.InternalServerError)

  }

  def responseMessage(msg: String): String = Try(JsonReader.toJson(Map[String, Any](
    KeyMessage -> msg
  ))) match {
    case Success(value)     => value
    case Failure(ex)        => s"""{"$KeyMessage": $msg; "internal_error:": "${ex.getMessage}"; }"""
  }

}
