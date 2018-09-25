package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.actions

import com.intel.cg.hpfd.csr.generated.parser_ext_r
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.Parser.ProtoOffsets
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output.PacketFlags


class ExtractAction(val protoId: Option[Short], val keyOffset: Short, val flagNum: Option[Short], val flagVal: Boolean, val ptrNum: Int) {

  def apply(input: (ProtoOffsets, PacketFlags)): (ProtoOffsets, PacketFlags) = {
    val flags: PacketFlags =
      flagNum match {
        case None => input._2
        case Some(extractedFlagNum) => input._2.assign(extractedFlagNum, flagVal)
      }
    val fields: ProtoOffsets = protoId match {
      case None => input._1
      case Some(x) => input._1.updated(ptrNum, (x.toInt, keyOffset.toInt))
    }
    (fields, flags)
  }

  override def toString: String = {
    s"pid = $protoId, koff = $keyOffset, flagnum = $flagNum, flagval = $flagVal, ptrNum = $ptrNum"
  }

}

object ExtractAction {

  val SpecialProtocolId = 0xff

  def apply(csr: parser_ext_r): ExtractAction = {
    // 0 is special as a flag number, means do _no_ flag annotation do not update _0_
    val flagNum = csr.FLAG_NUM() match {
      case 0L => None
      case x => Some(x.toShort)
    }

    // 0xff is  special proto-id, do no annotation to the fields in that case
    val protoId = csr.PROTOCOL_ID() match {
      case SpecialProtocolId => None
      case x => Some(x.toShort)
    }
    new ExtractAction(protoId, csr.OFFSET.toShort, flagNum, csr.FLAG_VALUE() == 1L, csr.PTR_NUM.toShort)
  }

}
