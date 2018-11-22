package madisonbay.fs2app.http.dispatcher

import madisonbay.iface.frontend.viewer.UriConstants.UriParameter
import spinoco.protocol.http.Uri

trait ProcessRequest {

  def getParameters(query: Uri.Query): List[UriParameter] = query.collect[UriParameter] {
    case Uri.QueryParameter.Single(k, v)  => UriParameter(k, Some(v))
    case Uri.QueryParameter.Flag(f)       => UriParameter(f, None)
  }

}
