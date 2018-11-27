package madisonbay.iface.rest

import madisonbay.iface.UriConstants.UriParameter
import madisonbay.iface.model.CsrModel
import spinoco.protocol.http.HttpStatusCode

object RestDispatcher {

  def processGetUri(uri: List[String], parameters: List[UriParameter], csrModel: CsrModel): RestResponse = {

    uri match {
      case CsrModel.KeyTopMap :: _ => CsrTreeRest.processGetRequest(uri, parameters, csrModel)

      case CsrAddressRest.UriAddress :: _ => CsrAddressRest.processGetRequest(uri, parameters, csrModel)

      case _ => RestResponse(uriSupported = false, error = false, response = "", HttpStatusCode.NotFound)
    }
  }

}
