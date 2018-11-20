package madisonbay.frontend.mbyhtml

object HtmlDsl {

  def html(head: String, body: String): String = HtmlTag + Endl + head + Endl + body + Endl + HtmlEnd

  def head(meta: List[String]): String = HeadTag + Endl + resolve(meta) + HeadEnd

  def body(content: List[String]): String = BodyTag + Endl + resolve(content) + BodyEnd

  val Endl = "\n"

  val HtmlTag = """<!DOCTYPE html>""" + Endl + "<html lang=\"en\">"
  val HtmlEnd = "</html>"

  val HeadTag = "<head>"
    val MetaCharset = "<meta charset=\"utf-8\" />"
  val HeadEnd = "</head>"

  val BodyTag = "<body>"
  val BodyEnd = "</body>"

  private def resolve(content: List[String]): String = content.mkString(Endl) + Endl

}
