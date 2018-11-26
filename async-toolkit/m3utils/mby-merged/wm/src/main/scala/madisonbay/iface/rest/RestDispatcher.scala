package madisonbay.iface.rest

import madisonbay.iface.UriConstants.UriParameter
import madisonbay.iface.model.CsrModel

object RestDispatcher {

  def processGetUri(uri: List[String], parameters: List[UriParameter], csrModel: CsrModel): RestResponse = {
    val _ = parameters

    uri match {
      case CsrModel.KeyTopMap :: _ => CsrRest.processGetRequest(uri, csrModel)

      case _ => RestResponse(uriSupported = false, error = false, response = None)
    }
  }

}
