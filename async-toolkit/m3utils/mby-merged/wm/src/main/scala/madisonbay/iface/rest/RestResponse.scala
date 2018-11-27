package madisonbay.iface.rest

import spinoco.protocol.http.HttpStatusCode

case class RestResponse(uriSupported: Boolean, error: Boolean, response: String, httpStatusCode: HttpStatusCode)

