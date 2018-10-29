//scalastyle:off regex.tuples
package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.actions

import madisonbay.csr.all._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.Parser.ProtoOffsets
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output.PacketFlags


class ExtractAction(registerExt: parser_ext_r) {

  def extract(input: (ProtoOffsets, PacketFlags)): (ProtoOffsets, PacketFlags) = {
    val fields: ProtoOffsets = registerExt.PROTOCOL_ID() match {
      case ExtractAction.SpecialProtocolId  => input._1
      case protoId => input._1.updated(registerExt.PTR_NUM().toShort, (protoId.toInt, registerExt.OFFSET().toInt))
    }
    val flags: PacketFlags = registerExt.FLAG_NUM() match {
      case ExtractAction.FlagNOP  => input._2
      case extractedFlagNum       => input._2.assign(extractedFlagNum.toInt, registerExt.FLAG_VALUE() == 1L)
    }
    (fields, flags)
  }

  def toStringRegExt: String = {
    val state = f"${registerExt.state}%x"
    s"$registerExt ($state) (FLAG_NUM=${registerExt.FLAG_NUM()}, FLAG_VALUE=${registerExt.FLAG_VALUE()}, " +
      s"PROTOCOL_ID=${registerExt.PROTOCOL_ID()}, PTR_NUM=${registerExt.PTR_NUM()}, OFFSET=${registerExt.OFFSET()})"
  }

}

object ExtractAction {

  // To avoid extracting a header pointer, set the protocol ID associated with the extraction to 0xFF
  val SpecialProtocolId = 0xffL

  // To avoid setting a parser flag, use the NOP flag number of 0 (implying that flag 0 is unusable)
  val FlagNOP = 0L

  def apply(registerExt: parser_ext_r): ExtractAction = new ExtractAction(registerExt)

}
