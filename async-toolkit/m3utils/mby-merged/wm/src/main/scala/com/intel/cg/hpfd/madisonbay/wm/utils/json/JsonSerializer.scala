package com.intel.cg.hpfd.madisonbay.wm.utils.json

import JsonValues.{isValue, toJsonValue}

import scala.util.Try

object JsonSerializer {

  def toMap(any: Any, bNameCaseClasses: Boolean = false): Map[String, Any] = {

    def isCaseClassField(field: java.lang.reflect.Field): Boolean = field.getName == "$outer"

    def isFieldSupported(field: java.lang.reflect.Field): Boolean = !isCaseClassField(field)

    def isUserCaseClass(ob: Any): Boolean = ob match {
      case _: List[_] => false
      case _: Product => true
      case _          => false
    }

    def toMapObject(ob: AnyRef): Any = {
      val fields = ob.getClass.getDeclaredFields
      val fieldsSize = fields.size
      if (fieldsSize == 0 || (fieldsSize == 1 && isCaseClassField(fields.head))) {
        if (isUserCaseClass(ob))  {ob.toString} else {ob.getClass.getSimpleName}
      } else {
        fields.foldLeft(Map[String, Any]()) { (acc, field) =>
          field.setAccessible(true)
          val value = field.get(ob)
          if (isFieldSupported(field) && value != null) {
            val name = if (bNameCaseClasses && isUserCaseClass(value)) {value.getClass.getSimpleName} else {field.getName}
            acc + (name -> recursiveToMap(value))
          } else {
            acc
          }
        }
      }
    }

    def recursiveToMap(el: Any): Any = el match {
      case v if isValue(v)      => toJsonValue(el)
      case map: Map[_, _]       => map.collect { case (k, v) => k -> recursiveToMap(v) }
      case col: Traversable[_]  => toJsonValue(col.map(recursiveToMap))
      case ob: AnyRef           => toMapObject(ob)
      case x => throw new IllegalArgumentException(s"Unsupported $x")
    }

    any match {
      case v if isValue(v) => throw new IllegalArgumentException(s"Value $v must be class instance")
      case col: Traversable[_] => throw new IllegalArgumentException(s"Value $col must be class instance")
      case v => recursiveToMap(v).asInstanceOf[Map[String, Any]]
    }

  }

  def toMapWithCaseClasses(any: Any): Map[String, Any] = toMap(any, bNameCaseClasses = true)

  def toMapTry(any: Any): Try[Map[String, Any]] = Try(toMap(any))

}
