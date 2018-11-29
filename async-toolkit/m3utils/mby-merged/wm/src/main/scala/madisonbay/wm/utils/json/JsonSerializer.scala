package madisonbay.wm.utils.json

import JsonValues.{isValue, toJsonValue}
import java.lang.reflect.Field


object JsonSerializer {

  def isInnerCaseClassField(field: Field): Boolean = field.getName == "$outer"

  val StandardFieldFilter: (Field, AnyRef) => Boolean = (field, _) => !isInnerCaseClassField(field)

  val StandardFieldSpecialTreatment: (Field, AnyRef) => Option[Any] = (_, _) => None

  def toMapStd(value: Any): Map[String, Any] = toMap(value, bNameCaseClasses = false)(StandardFieldFilter)

  def toMapCaseClassNameStd(any: Any): Map[String, Any] = toMap(any, bNameCaseClasses = true)(StandardFieldFilter)

  //scalastyle:off cyclomatic.complexity
  def toMap(any: Any, bNameCaseClasses: Boolean)(
    filterField: (Field, AnyRef) => Boolean,
    specialFieldTreatment: (Field, AnyRef) => Option[Any] = StandardFieldSpecialTreatment): Map[String, Any] = {

    def isUserCaseClass(ob: Any): Boolean = ob match {
      case _: List[_]   => false
      case _: Option[_] => false
      case _: Product   => true
      case _            => false
    }

    def toMapObject(ob: AnyRef): Any = {
      val fields = ob.getClass.getDeclaredFields
      val fieldsSize = fields.size
      if (fieldsSize == 0 || (fieldsSize == 1 && isInnerCaseClassField(fields.head))) {
        if (isUserCaseClass(ob))  {ob.toString} else {ob.getClass.getSimpleName}
      } else {
        fields.foldLeft(Map[String, Any]()) { (acc, field) =>
          field.setAccessible(true)
          val value = field.get(ob)
          if (value != null && filterField(field, value)) {
            val name = if (bNameCaseClasses && isUserCaseClass(value)) {value.getClass.getSimpleName} else {field.getName}
            specialFieldTreatment(field, value) match {
              case Some(v) => acc + (name -> v)
              case None => acc + (name -> recursiveToMap(value))
            }
          } else {
            acc
          }
        }
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
