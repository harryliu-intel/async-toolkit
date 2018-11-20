package madisonbay.frontend.mbyhtml

object HtmlDsl {

  def html(head: String, body: String): String = HtmlTag + Endl + head + Endl + body + Endl + HtmlEnd

  def head(meta: List[String]): String = HeadTag + Endl + resolve(meta) + HeadEnd

  def body(content: List[String]): String = BodyTag + Endl + resolve(content) + BodyEnd

  def divTag(id: String, content: List[String]): String = "<div id=\"" + id + "\">" + Endl + resolve(content) + DivEnd

  val Endl = "\n"

  val HtmlTag = """<!DOCTYPE html>""" + Endl + "<html lang=\"en\">"
  val HtmlEnd = "</html>"

  val HeadTag = "<head>"
    val MetaCharset = "<meta charset=\"utf-8\" />"
    val LinkCss = "<link rel=\"stylesheet\" type=\"text/css\" href=\"css/style.css\" />"
  val HeadEnd = "</head>"

  val BodyTag = "<body>"
  val BodyEnd = "</body>"

  val DivEnd = "</div>"

  private def resolve(content: List[String]): String = content.mkString(Endl) + Endl

}
