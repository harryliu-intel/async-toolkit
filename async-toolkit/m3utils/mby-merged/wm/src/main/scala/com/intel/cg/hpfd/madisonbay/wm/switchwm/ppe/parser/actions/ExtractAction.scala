//scalastyle:off regex.tuples
package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.actions

import madisonbay.csr.all._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.Parser.{HeaderPointer, ProtoOffsets}
import com.intel.cg.hpfd.madisonbay.wm.utils.BitFlags


class ExtractAction(registerExt: parser_ext_r) {

  def extract(protoOffsets: ProtoOffsets, parserFlags: BitFlags): (ProtoOffsets, BitFlags) = {
    val fields: ProtoOffsets = registerExt.PROTOCOL_ID() match {
      case ExtractAction.SpecialProtocolId  => protoOffsets
      case protoId => protoOffsets.updated(registerExt.PTR_NUM().toShort, HeaderPointer(protoId.toInt, registerExt.OFFSET().toInt))
    }
    val flags: BitFlags = registerExt.FLAG_NUM() match {
      case ExtractAction.FlagNOP  => parserFlags
      case extractedFlagNum       => parserFlags.assign(extractedFlagNum.toInt, registerExt.FLAG_VALUE() == 1L)
    }
    (fields, flags)
  }

  def toStringRegExt: String = {
    val state = f"${registerExt.companion._state.get(registerExt).extract[Long]}%X"
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

  def extractActions(actions: List[ExtractAction], protoOffsets: ProtoOffsets, packetFlags: BitFlags): (ProtoOffsets, BitFlags) =
    actions.foldLeft(protoOffsets, packetFlags) { (accumulator, act) => act.extract(accumulator._1, accumulator._2) }

}
