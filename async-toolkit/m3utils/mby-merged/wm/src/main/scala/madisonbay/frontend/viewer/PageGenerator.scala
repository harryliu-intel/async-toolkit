package madisonbay.frontend.viewer

import madisonbay.frontend.model.Model
import Parameters.UriParameter
import madisonbay.frontend.viewer.HtmlDsl._


object PageGenerator {

  def mainPage(parameters: List[UriParameter]): String = processGet(List(), parameters)

  def processGet(uriPath: List[String], parameters: List[UriParameter]): String = uriPath match {
    case Nil  => defaultPage(showCsr(parameters))
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

  def showCsr(parameters: List[UriParameter]): List[String] = parameters.find(_.key == Parameters.KeyPath) match {
    case Some(UriParameter(_, Some(path))) => CsrView.genView(Model.csrMap, path)
    case _ => CsrView.genView(Model.csrMap, Model.topMapKey)
  }

}
