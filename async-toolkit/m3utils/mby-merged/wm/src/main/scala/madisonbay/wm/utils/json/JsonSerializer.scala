package madisonbay.wm.utils.json

import JsonValues.{isValue, toJsonValue}
import java.lang.reflect.Field


object JsonSerializer {

  def isInnerCaseClassField(field: Field): Boolean = field.getName == "$outer"

  val StandardFieldFilter: (Field, AnyRef) => Boolean = (field, _) => !isInnerCaseClassField(field)

  val StandardFieldSpecialTreatment: (Map[String, Any], Field, AnyRef) => (Map[String, Any], Option[Any]) = (acc, _, _) => (acc, None)

  val StandardObjectTreatment: (Map[String, Any], Any) => Map[String, Any] = (result, _) => result

  def toMapStd(value: Any): Map[String, Any] = toMap(value, bNameCaseClasses = false)(StandardFieldFilter)

  def toMapCaseClassNameStd(any: Any): Map[String, Any] = toMap(any, bNameCaseClasses = true)(StandardFieldFilter)

  //scalastyle:off cyclomatic.complexity
  def toMap(any: Any, bNameCaseClasses: Boolean)(
    filterField: (Field, AnyRef) => Boolean,
    specialFieldTreatment: (Map[String, Any], Field, AnyRef) => (Map[String, Any], Option[Any]) = StandardFieldSpecialTreatment,
    objectTreatment: (Map[String, Any], Any) => Map[String, Any] = StandardObjectTreatment)
  : Map[String, Any] = {

    def isUserCaseClass(ob: Any): Boolean = ob match {
      case _: List[_]   => false
      case _: Option[_] => false
      case _: Product   => true
      case _            => false
    }

    def toMapObject(obj: AnyRef): Any = {
      val fields = obj.getClass.getDeclaredFields
      val fieldsSize = fields.size
      if (fieldsSize == 0 || (fieldsSize == 1 && isInnerCaseClassField(fields.head))) {
        if (isUserCaseClass(obj))  {obj.toString} else {obj.getClass.getSimpleName}
      } else {
        val result = fields.foldLeft(Map[String, Any]()) { (acc, field) =>
          field.setAccessible(true)
          val value = field.get(obj)
          if (value != null && filterField(field, value)) {
            val name = if (bNameCaseClasses && isUserCaseClass(value)) {value.getClass.getSimpleName} else {field.getName}
            specialFieldTreatment(acc, field, value) match {
              case (actResult, Some(v)) => actResult + (name -> v)
              case (actResult, None) => actResult + (name -> recursiveToMap(value))
            }
          } else {
            acc
          }
        }
        objectTreatment(result, obj)
      }
    }

    def recursiveToMap(el: Any): Any = el match {
      case v if isValue(v)      => toJsonValue(el)
      case opt: Option[_] if opt.isEmpty => Map[String, Any]()
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

}
