package madisonbay.iface.rest

import madisonbay.iface.UriConstants.UriParameter
import madisonbay.iface.model.CsrModel
import madisonbay.iface.rest.Constants._
import madisonbay.wm.utils.json.JsonReader
import spinoco.protocol.http.HttpStatusCode

object CsrAddressRest extends RestProcessing {

  val UriAddress = "address"

  def processGetRequest(uri: List[String], parameters: List[UriParameter], csrModel: CsrModel): RestResponse = uri match {

    case UriAddress :: address :: Nil if JsonReader.isInteger(address) => csrModel.csr.getRegister(address.toLong) match {

      case Some(value) => returnJson(Map(
        Keys.KeyType -> Types.TypeRegister,
        Keys.KeyValue -> value.toLong,
        Keys.KeyRawValue -> value.toRaw
        ))

      case None => RestResponse(uriSupported = true, error = false,
        responseMessage(s"Value under address $address bytes not found"), HttpStatusCode.NotFound)
    }

    case _ => RestResponse(uriSupported = false, error = false,
      responseMessage(s"URI ${uriPath(uri)} not supported"), HttpStatusCode.NotFound)

  }

}
