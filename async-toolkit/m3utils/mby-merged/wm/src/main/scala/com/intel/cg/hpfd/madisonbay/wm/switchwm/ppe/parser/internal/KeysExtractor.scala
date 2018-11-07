package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.internal

import com.intel.cg.hpfd.madisonbay.wm.switchwm.csr.Csr.CsrParser
import com.intel.cg.hpfd.madisonbay.wm.switchwm.epl.PacketHeader
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.Parser.{ExtractionIndex, HeaderPointer, ProtoOffsets}
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output.PacketFields
import com.intel.cg.hpfd.madisonbay.wm.utils.extensions.UIntegers.getLower32
import com.intel.cg.hpfd.madisonbay.wm.utils.extensions.ExtLong.Implicits
import madisonbay.csr.all._

object KeysExtractor {

  def extractKeys(csrParser: CsrParser, packetHeader: PacketHeader, protoOffsets: ProtoOffsets, extractionIndex: ExtractionIndex):
      (PacketFields, CsrParser) = {

    val extractorCsr = csrParser.ppeParserMap.PARSER_EXTRACT_CFG(extractionIndex).PARSER_EXTRACT_CFG
    val countersL  = mby_ppe_parser_map._PARSER_COUNTERS
    val ext_unknown_protid = countersL composeLens parser_counters_r._EXT_UNKNOWN_PROTID composeLens parser_counters_r.EXT_UNKNOWN_PROTID._value
    val ext_dup_protid = countersL composeLens parser_counters_r._EXT_DUP_PROTID composeLens parser_counters_r.EXT_DUP_PROTID._value

    def modify(actualState: (Map[Int, Short], mby_ppe_parser_map, Int), parserExtractCfgReg: parser_extract_cfg_r) = {
      val (result, mbyPpeParserMap, counter) = actualState
      val protocolId = parserExtractCfgReg.PROTOCOL_ID()
      def toWordWithOffset(v: Int): Short = packetHeader.getWord(v + getLower32(parserExtractCfgReg.OFFSET()).toInt)

      (protocolId, protoOffsets.collect { case (_, HeaderPointer(pId, baseOffset)) if pId == protocolId => baseOffset }.toList) match {

        case (ExtractAction.SpecialProtocolId, _) => (result, mbyPpeParserMap, counter + 1)

        case (_, Nil) =>
          val next = ext_unknown_protid.modify((v: Long) => v.incWithUByteSaturation)
          (result, next(mbyPpeParserMap), counter + 1)

        case (_, h :: Nil) =>
          (result.updated(counter, toWordWithOffset(h)), mbyPpeParserMap, counter + 1)

        case (_, h :: _) =>
          val next = ext_dup_protid.modify((v: Long) => v.incWithUByteSaturation)
          (result.updated(counter, toWordWithOffset(h)), next(mbyPpeParserMap), counter + 1)

      }
    }

    val (result, updatedParserMap, _) = extractorCsr.foldLeft((Map[Int, Short](), csrParser.ppeParserMap, 0))(modify)

    (PacketFields(result), csrParser.copy(ppeParserMap = updatedParserMap))
  }

}
