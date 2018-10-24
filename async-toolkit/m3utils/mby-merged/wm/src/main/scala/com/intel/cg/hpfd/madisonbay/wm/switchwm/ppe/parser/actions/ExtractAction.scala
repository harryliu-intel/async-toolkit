//scalastyle:off regex.tuples
package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.actions

import com.intel.cg.hpfd.csr.generated.parser_ext_r
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.Parser.ProtoOffsets
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output.PacketFlags


class ExtractAction(protocolId: Option[Short], keyOffset: Short, flagNum: Option[Short], flagVal: Boolean, ptrNum: Int) {

  def extract(input: (ProtoOffsets, PacketFlags)): (ProtoOffsets, PacketFlags) = {
    val flags: PacketFlags = flagNum match {
        case None                   => input._2
        case Some(extractedFlagNum) => input._2.assign(extractedFlagNum, flagVal)
      }
    val fields: ProtoOffsets = protocolId match {
      case None           => input._1
      case Some(protoId)  => input._1.updated(ptrNum, (protoId.toInt, keyOffset.toInt))
    }
    (fields, flags)
  }

  override def toString: String = s"pid = $protocolId, koff = $keyOffset, flagnum = $flagNum, flagval = $flagVal, ptrNum = $ptrNum"

}

object ExtractAction {

  // To avoid extracting a header pointer, set the protocol ID associated with the extraction to 0xFF
  val SpecialProtocolId = 0xff

  // To avoid setting a parser flag, use the NOP flag number of 0 (implying that flag 0 is unusable)
  val FlagNOP = 0L

  def apply(csr: parser_ext_r.parser_ext_r): ExtractAction = {
    // 0 is special as a flag number, means do _no_ flag annotation do not update _0_
    val flagNum = csr.FLAG_NUM() match {
      case FlagNOP => None
      case x => Some(x.toShort)
    }

    // 0xff is  special proto-id, do no annotation to the fields in that case
    val protocolId = csr.PROTOCOL_ID() match {
      case SpecialProtocolId  => None
      case protoId            => Some(protoId.toShort)
    }
    new ExtractAction(protocolId, csr.OFFSET().toShort, flagNum, csr.FLAG_VALUE() == 1L, csr.PTR_NUM().toShort)
  }

}
