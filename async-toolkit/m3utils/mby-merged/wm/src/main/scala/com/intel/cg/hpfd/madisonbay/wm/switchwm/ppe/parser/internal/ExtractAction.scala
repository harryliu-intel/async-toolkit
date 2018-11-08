//scalastyle:off regex.tuples
package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.internal

import madisonbay.csr.all._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output.ProtocolsOffsets
import com.intel.cg.hpfd.madisonbay.wm.utils.BitFlags


class ExtractAction(registerExt: parser_ext_r) {

  def extract(protoOffsets: ProtocolsOffsets, parserFlags: BitFlags): (ProtocolsOffsets, BitFlags) = {
    val fields: ProtocolsOffsets = registerExt.PROTOCOL_ID() match {
      case ExtractAction.SpecialProtocolId  => protoOffsets
      case protoId => protoOffsets.updated(registerExt.PTR_NUM().toShort, protoId.toInt, registerExt.OFFSET().toInt)
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

  val OffsetOfNextExtractAction   = 16

  // To avoid extracting a header pointer, set the protocol ID associated with the extraction to 0xFF
  val SpecialProtocolId = 0xffL

  // To avoid setting a parser flag, use the NOP flag number of 0 (implying that flag 0 is unusable)
  val FlagNOP = 0L

  def apply(registerExts: List[parser_ext_r]): List[ExtractAction] =
    List(new ExtractAction(registerExts.head), new ExtractAction(registerExts(OffsetOfNextExtractAction)))

  def extractActions(actions: List[ExtractAction], protoOffsets: ProtocolsOffsets, packetFlags: BitFlags): (ProtocolsOffsets, BitFlags) =
    actions.foldLeft(protoOffsets, packetFlags) { (accumulator, act) => act.extract(accumulator._1, accumulator._2) }

}
