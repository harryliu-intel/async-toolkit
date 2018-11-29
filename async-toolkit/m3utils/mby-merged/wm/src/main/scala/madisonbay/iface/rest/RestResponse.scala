package madisonbay.iface.rest

import madisonbay.iface.model.CsrModel
import spinoco.protocol.http.HttpStatusCode

case class RestResponse(
                         uriSupported: Boolean,
                         error: Boolean,
                         response: String,
                         httpStatusCode: HttpStatusCode,
                         updatedCsr: Option[CsrModel]
                       )

