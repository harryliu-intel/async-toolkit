package madisonbay.iface.rest.services

import madisonbay.iface.UriConstants.{UriParameter, UriSegment}
import madisonbay.iface.model.CsrModel
import madisonbay.iface.rest.Constants.{Keys, Types}
import madisonbay.iface.rest.{RestProcessing, RestResponse}
import madisonbay.wm.utils.json.JsonReader
import spinoco.protocol.http.HttpStatusCode
import RestProcessing.responseMessage

object CsrAddressRest extends RestProcessing {

  def processGetRequest(uri: List[String], parameters: List[UriParameter], csrModel: CsrModel): RestResponse = uri match {

    case UriSegment.Address :: address :: Nil if JsonReader.isInteger(address) => csrModel.csr.getRegister(address.toLong) match {

      case Some(value) => returnJson(Map(
        Keys.KeyType -> Types.TypeRegister,
        Keys.KeyValue -> value.toLong,
        Keys.KeyRawValue -> value.toRaw
        ))

      case None => RestResponse(uriSupported = true, error = false,
        responseMessage(s"Value under address $address bytes not found"), HttpStatusCode.NotFound, None)
    }

    case _ => RestResponse(uriSupported = false, error = false,
      responseMessage(s"URI ${uriPath(uri)} not supported"), HttpStatusCode.NotFound, None)

  }

}
