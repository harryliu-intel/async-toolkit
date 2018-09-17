package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe

import com.intel.cg.hpfd.csr.generated.parser_ext_r
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.Parser.ProtoOffsets

class ExtractAction(val protoId : Option[Short], val keyOffset : Short, val flagNum : Option[Short], val flagVal : Boolean, val ptrNum : Int) {
  def apply (input : (ProtoOffsets, PacketFlags)) : (ProtoOffsets, PacketFlags) = {
    val flags : PacketFlags = flagNum match {
      case None => input._2
      case Some(flagNum) => input._2.assign(flagNum, flagVal)
    }
    val fields : ProtoOffsets = protoId match {
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
  def apply(csr : parser_ext_r) = {
    // 0 is special as a flag number, means do _no_ flag annotation do not update _0_
    val flagNum = csr.FLAG_NUM() match {
      case 0l => None
      case x => Some(x.toShort)
    }
    // 0xff is  special proto-id, do no annotation to the fields in that case
    val protoId = csr.PROTOCOL_ID() match {
      case 0xff => None
      case x => Some(x.toShort)
    }
    new ExtractAction(protoId, csr.OFFSET.toShort, flagNum, csr.FLAG_VALUE() == 1l, csr.PTR_NUM.toShort)
  }
}
