package madisonbay.iface.rest


case class RestResponse(uriSupported: Boolean, error: Boolean, response: Option[String])

