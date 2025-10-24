// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package madisonbay.iface.model

import madisonbay.wm.switchwm.csr.Csr
import madisonbay.wm.utils.json.JsonSerializer
import madisonbay.wm.utils.json.JsonSerializer.isInnerCaseClassField

import scala.util.{Success, Try}


object CsrModel {

  val KeyTopMap: String = Csr().topMap.getClass.getSimpleName

  val KeyValue  = "value"
  val KeyRange  = "range"
  val KeyType   = "type"

  val TypeAddressMap    = "AddressMap"
  val TypeRegisterFile  = "RegisterFile"
  val TypeRegister      = "Register"
  val TypeRegisterField = "RegisterField"
}

class CsrModel(val csr: Csr, limitNumberOfNodes: Int) {
  import CsrModel._

  val csrMap: Map[String, Any] = Map(CsrModel.KeyTopMap -> toCsrMap(csr.topMap))

  val csrMapToHtml: Map[String, Any] = joinValues(csrMap)

  def idMgpOpt(id: Int): Option[Int] = if (id >=0 && id < csr.topMap.mpp(0).mgp.length) { Some(id) } else { None }

  def idMgpOpt(id: String): Option[Int] = Try(id.toInt) match {
    case Success(idMgp) => idMgpOpt(idMgp)
    case _ => None
  }

  private def toCsrMap(obj: Any): Map[String, Any] =
    JsonSerializer.toMap(obj, bNameCaseClasses = false) (filterField = {
      case (field, _) if isInnerCaseClassField(field) => false
      case (field, _) if isInternalField(field.getName) => false
      // limit is due to huge register tree; to disable it, set limitNumberOfNodes <= 0
      case (_, value: Seq[_]) if limitNumberOfNodes > 0 && value.length > limitNumberOfNodes => false
      case _ => true
    }, objectTreatment = (resultMap, obj) => obj.getClass.getName match {
      case addrMap if addrMap.endsWith("_map") => resultMap + (KeyType -> TypeAddressMap)
      case regFile if regFile.endsWith("_rf") => resultMap + (KeyType -> TypeRegisterFile)
      case reg if reg.endsWith("_r") => resultMap + (KeyType -> TypeRegister)
      case _ => resultMap
    })

  private def joinValues(map: Map[String, Any]): Map[String, Any] = map.collect {
    case (k, v: Map[_, _]) =>
      val vmap = v.asInstanceOf[Map[String, Any]]
      vmap.get(KeyValue) match {
        case Some(value: Long) => k -> value
        case _ => k -> joinValues(vmap)
      }

    case (k, v: List[_]) => k -> v.map {
      case m: Map[_, _] => joinValues(m.asInstanceOf[Map[String, Any]])
      case any => any
      }

    case (k, v) => k -> v
  }

  private def isInternalField(name: String): Boolean = name.startsWith("bitmap") ||
    name == "companion" ||
    name == "resetValue"

}
