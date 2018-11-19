package madisonbay.wm.utils

import madisonbay.wm.utils.json.JsonReader

import scala.io.Source
import scala.util.Try

object Loader {

  def loadJson(filename: String): Try[Map[String, Any]] =
    for {
      src <- Try(Source.fromFile(filename))
      result <- JsonReader.parse(src.getLines().toList.mkString(""))
    } yield result

}
