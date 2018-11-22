package madisonbay.iface.frontend.viewer

import madisonbay.iface.frontend.model.CsrModel
import UriConstants._
import madisonbay.iface.frontend.viewer.HtmlDsl._


object PageGenerator {

  def mainPage(parameters: List[UriParameter], csrModel: CsrModel): String = processGet(List(), parameters, csrModel)

  def processGet(uriPath: List[String], parameters: List[UriParameter], csrModel: CsrModel): String = uriPath match {
    case Nil  => defaultPage(showCsr(parameters, csrModel))
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

  def showCsr(parameters: List[UriParameter], csrModel: CsrModel): List[String] = parameters.find(_.key == UriParameter.KeyPath) match {
    case Some(UriParameter(_, Some(path))) => CsrView.genView(csrModel.csrMapToHtml, path)
    case _ => CsrView.genView(csrModel.csrMapToHtml, CsrModel.topMapKey)
  }

}
