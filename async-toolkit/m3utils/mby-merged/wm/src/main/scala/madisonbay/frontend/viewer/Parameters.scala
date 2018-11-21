package madisonbay.frontend.viewer

object Parameters {

  case class UriParameter(key: String, value: Option[String])

  val KeyPath = "path"

  val IndexOpen = "/"
  val IndexClose = "-"

}
