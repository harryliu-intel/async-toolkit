package madisonbay.frontend.model

import madisonbay.wm.switchwm.csr.Csr
import madisonbay.wm.utils.json.JsonSerializer
import madisonbay.wm.utils.json.JsonSerializer.isInnerCaseClassField

object Model {

  val topMapKey = "top_map"

  val csr: Csr = Csr()

  val csrMap: Map[String, Any] = Map(topMapKey -> toCsrMap(csr.topMap))

  def toCsrMap(obj: Any): Map[String, Any] = {
    val map = JsonSerializer.toMap(obj, bNameCaseClasses = false) {
      case (field, _) if isInnerCaseClassField(field) => false
      case (field, _) if isInternalField(field.getName) => false
        // TODO: limit is due to huge register tree
      case (_, value: Seq[_]) if value.length > 32 => false
      case _ => true
    }
    joinValues(map)
  }

  private def joinValues(map: Map[String, Any]): Map[String, Any] = map.collect {
    case (k, v: Map[_, _]) =>
      val vmap = v.asInstanceOf[Map[String, Any]]
      vmap.get("value") match {
        case Some(value: Long) => k -> value
        case _ => k -> joinValues(vmap)
      }

    case (k, v: List[_]) => k -> v.map {
      case m: Map[_, _] => joinValues(m.asInstanceOf[Map[String, Any]])
      case any => any
      }

    case (k, v) => k -> v
  }

  private def isInternalField(name: String): Boolean = name.startsWith("bitmap") || name == "companion" ||
    name == "range" || name == "resetValue"

}
