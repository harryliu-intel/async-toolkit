package madisonbay.iface.rest

import madisonbay.iface.UriConstants.{UriParameter, UriSegment}
import madisonbay.iface.model.CsrModel
import madisonbay.iface.rest.services.{CsrAddressRest, CsrTreeRest, ParserRest, RestProcessing}
import madisonbay.wm.utils.json.JsonReader
import spinoco.protocol.http.HttpStatusCode

object RestDispatcher {

  def processGetUri(uri: List[String], parameters: List[UriParameter], csrModel: CsrModel): RestResponse = {

    uri match {
      case CsrModel.KeyTopMap :: _ => CsrTreeRest.processGetRequest(uri, parameters, csrModel)

      case UriSegment.Address :: _ => CsrAddressRest.processGetRequest(uri, parameters, csrModel)

      case _ => RestResponse(uriSupported = false, error = false, response = "", HttpStatusCode.NotFound, None)
    }
  }

  def processPostJson(uri: List[String], parameters: List[UriParameter], jsonMap: Map[String, Any], csrModel: CsrModel): RestResponse = {
    uri match {
      case CsrModel.KeyTopMap :: _ => CsrTreeRest.processGetRequest(uri, parameters, csrModel)

      case UriSegment.Address :: _ => CsrAddressRest.processGetRequest(uri, parameters, csrModel)

      case UriSegment.Parser :: _ => ParserRest.processPostJson(uri, jsonMap, csrModel)

        // TODO: demo only
      case "test" :: "div" :: Nil => testForDemo(jsonMap)

      case _ => RestResponse(uriSupported = false, error = false, response = "", HttpStatusCode.NotFound, None)
    }
  }

  private def testForDemo(jsonMap: Map[String, Any]): RestResponse = {
    (JsonReader.getIntOpt(jsonMap, "input.a"), JsonReader.getIntOpt(jsonMap, "input.b")) match {
      case (Some(a), Some(b)) =>
        if (b == 0) {
          RestResponse(uriSupported = true, error = true,
            response = RestProcessing.responseMessage("Division by zero!"),
            HttpStatusCode.Ok, None)
        } else {
          RestResponse(uriSupported = true, error = false,
            response = JsonReader.toJson(Map[String, Any](
              "output"->Map(
                "a" -> a/b,
                "b" -> a%b
              )
            )),
            HttpStatusCode.Ok, None)
        }

      case _ =>  RestResponse(uriSupported = true, error = true,
        response = RestProcessing.responseMessage("Unable to read input"),
        HttpStatusCode.Ok, None)
    }
  }
}
