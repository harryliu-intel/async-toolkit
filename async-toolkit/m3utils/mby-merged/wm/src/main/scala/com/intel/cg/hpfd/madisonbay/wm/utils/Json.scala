package com.intel.cg.hpfd.madisonbay.wm.utils

import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.databind.ObjectMapper

object Json {

  def parse(strJson: String): Map[String, Any] = {
    val mapper = new ObjectMapper
    mapper.registerModule(DefaultScalaModule)
    mapper.readValue(strJson, classOf[Map[String,Any]])
  }

}
