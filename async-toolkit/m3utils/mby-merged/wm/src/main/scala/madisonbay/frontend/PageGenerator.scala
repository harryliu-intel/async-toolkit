package madisonbay.frontend

import madisonbay.fs2app.http.IoUriDispatcher.UriParameter
import mbyhtml.HtmlDsl._

object PageGenerator {

  def mainPage(parameters: List[UriParameter]): String = processGet(List(), parameters)

  def processGet(uriPath: List[String], parameters: List[UriParameter]): String = uriPath match {
    case Nil  => pageTemplate(List("Main Page"))
    case _    => pageTemplate(List("GET:", uriPath.mkString("/"), parameters.mkString(",")))
  }

  def pageTemplate(htmlBody: List[String]): String =
    html(
      head(List(
        MetaCharset
      )),
      body(
        htmlBody
      )
    )

}
