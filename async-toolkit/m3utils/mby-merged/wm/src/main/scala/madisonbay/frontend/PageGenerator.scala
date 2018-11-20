package madisonbay.frontend

import madisonbay.fs2app.http.IoUriDispatcher.UriParameter
import mbyhtml.HtmlDsl._

object PageGenerator {

  def mainPage(parameters: List[UriParameter]): String = processGet(List(), parameters)

  def processGet(uriPath: List[String], parameters: List[UriParameter]): String = uriPath match {
    case Nil  => defaultPage(List("Main Page"))
    case _    => defaultPage(List("GET:", uriPath.mkString("/"), parameters.mkString(",")))
  }

  def pageTemplate(htmlBody: List[String]): String =
    html(
      head(List(
        MetaCharset,
        LinkCss
      )),
      body(
        htmlBody
      )
    )

  def defaultPage(htmlBody: List[String]): String = pageTemplate(
    divTag("titlebar", List("Madison Bay Switch")) :: divTag("content", htmlBody) :: Nil
  )

}
