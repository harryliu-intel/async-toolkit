package com.intel.cg.hpfd.madisonbay.wm.utils.progparser

import com.intel.cg.hpfd.madisonbay.wm.utils.Json

import scala.io.Source
import scala.util.Try

object Loader {

  def loadJson(filename: String): Try[Map[String, Any]] =
    Try {
      val src = Source.fromResource(filename)
      val result = Json.parse(src.getLines().toList.mkString(""))
      src.close()
      result
    }

}
