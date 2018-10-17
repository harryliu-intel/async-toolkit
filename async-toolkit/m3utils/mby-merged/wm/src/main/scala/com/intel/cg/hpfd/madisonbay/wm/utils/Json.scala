package com.intel.cg.hpfd.madisonbay.wm.utils

import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.databind.ObjectMapper

import scala.util.Try


object Json {

  def parse(strJson: String): Try[Map[String, Any]] = Try {
    val mapper = new ObjectMapper
    mapper.registerModule(DefaultScalaModule)
    mapper.readValue(strJson, classOf[Map[String,Any]])
  }

  val PatternListApplyElement = "[a-zA-Z_][a-zA-Z_0-9]*([\\(][0-9]+[\\)])+"

  //scalastyle:off cyclomatic.complexity
  def getOpt(json: Map[String, Any], path: String): Option[Any] = {
    def getRec(res: Option[Any], keys: List[String], listIds: List[String]): Option[Any] =
      if (listIds.nonEmpty) {
        res match {
          case Some(l: List[_]) => getRec(l.lift(listIds.head.toInt), keys, listIds.tail)
          case _ => None
        }
      } else {
        keys match {
          case Nil => res
          case hKey :: tailKeys if hKey.startsWith("#") => getRec(res, tailKeys, hKey.split("#").toList.tail)
          case hKey :: tailKeys =>
            res match {
              case Some(m: Map[_, _]) => getRec(m.asInstanceOf[Map[String, Any]].get(hKey), tailKeys, List.empty)
              case Some(_) => None
              case None => None
            }
        }
      }
    getRec(Some(json), compilePath(path), List.empty)
  }
  //scalastyle:on cyclomatic.complexity


  // abc.def(3)(5).ghi(2).jkl  =>  List(abc, def, #3#5, ghi, #2, jkl)
  def compilePath(path: String): List[String] = path.split("\\.").toList.
    flatMap (s =>
      if (s.matches(PatternListApplyElement)) {
        s.replaceAll("\\)", "").split("\\(").toList match {
          case name :: ids => List(name, "#" + ids.mkString("#"))
          case _ => List(s)
        }
      } else {
        List(s)
      }
    )

}
