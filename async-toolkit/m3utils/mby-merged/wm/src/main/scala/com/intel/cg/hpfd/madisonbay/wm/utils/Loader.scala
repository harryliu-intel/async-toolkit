package com.intel.cg.hpfd.madisonbay.wm.utils

import scala.io.Source
import scala.util.Try

object Loader {

  def loadJson(filename: String): Try[Map[String, Any]] =
    for {
      src <- Try(Source.fromResource(filename))
      result <- Json.parse(src.getLines().toList.mkString(""))
      _ <- Try(src.close())
    } yield result

}
