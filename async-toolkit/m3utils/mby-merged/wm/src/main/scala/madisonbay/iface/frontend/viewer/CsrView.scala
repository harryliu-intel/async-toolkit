package madisonbay.iface.frontend.viewer

import madisonbay.wm.utils.json.JsonReader
import UriConstants.UriParameter._
import HtmlDsl._

object CsrView {

  val PathHref = s"/?$KeyPath="

  def genView(csrTree: Map[String, Any], path: String): List[String] = {

    val prevPath = getPathToRoot(path)

    val regView = s"Register View: $BR$BR"

    val regBody = getRegBody(csrTree, path)

    regView :: prevPath :: TableTag :: regBody ::: List(TableEnd)
  }

  private def getPathToRoot(path: String): String = path.split('/').toList match {
    case Nil => ""
    case _ :: Nil => ""
    case list =>
      val rootPath = if (list.last.matches(JsonReader.PatternNumber)) {
        list.dropRight(2).mkString("/")   // if path ends with number drop last key and index
      } else {
        list.dropRight(1).mkString("/")
      }
      s"${aHref(PathHref + rootPath, "<< Back << ")} $BR $BR"
  }

  private def getRegBody(csrTree: Map[String, Any], uriPath: String): List[String]= JsonReader.getOptFromUri(csrTree, uriPath) match {
    case Some(node: Map[_, _]) => node.asInstanceOf[Map[String, Any]].toList.map {
      case (k, v: Long)     => field(k, v.toString)

      case (k, _: Map[_,_]) =>
        val pathToK = s"$PathHref$uriPath/$k"
        field(k, aHref(pathToK, "{...}"))

      case (k, l: List[_])  =>
        val elements = l.zipWithIndex.map { case (_, index) =>
            val pathToId = s"$PathHref$uriPath/$k/$index"
            aHref(pathToId, index.toString)
          }.mkString(", ")
        field(k, "[" + elements + "]")

      case (k, _) => s"unsupported key $k $BR"
    }

    case _ => List()
  }

  private def field(key: String, href: String): String = s"$TrTag$TdTag$key:$TdEnd$TdTag$href$TdEnd$TrEnd$Endl"

}
