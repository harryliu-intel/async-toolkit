package madisonbay.iface.frontend.model

import madisonbay.wm.switchwm.csr.Csr
import madisonbay.wm.utils.json.JsonSerializer
import madisonbay.wm.utils.json.JsonSerializer.isInnerCaseClassField


object CsrModel {

  val topMapKey = "mby_top_map"

}

class CsrModel(csr: Csr, limitNumberOfNodes: Int) {

  val csrMap: Map[String, Any] = Map(CsrModel.topMapKey -> toCsrMap(csr.topMap))

  val csrMapToHtml: Map[String, Any] = joinValues(csrMap)

  def toCsrMap(obj: Any): Map[String, Any] =
    JsonSerializer.toMap(obj, bNameCaseClasses = false) {
      case (field, _) if isInnerCaseClassField(field) => false
      case (field, _) if isInternalField(field.getName) => false
      // limit is due to huge register tree; to disable it, set limitNumberOfNodes <= 0
      case (_, value: Seq[_]) if limitNumberOfNodes > 0 && value.length > limitNumberOfNodes => false
      case _ => true
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
