package madisonbay.iface.frontend.viewer

object UriConstants {

  case class UriParameter(key: String, value: Option[String])

  object UriParameter {
    val KeyPath = "path"

    val IndexOpen = "/"
    val IndexClose = "-"
  }

  object UriSegment {
    val CssUriPref = "css"
    val ImgUriPref = "img"
  }

}
