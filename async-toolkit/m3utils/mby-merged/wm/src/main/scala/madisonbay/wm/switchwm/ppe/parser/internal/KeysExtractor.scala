package madisonbay.wm.switchwm.ppe.parser.internal

import madisonbay.wm.switchwm.csr.Csr.CsrParser
import madisonbay.wm.switchwm.epl.PacketHeader
import madisonbay.wm.switchwm.ppe.parser.output.{PacketFields, ProtocolsOffsets}
import madisonbay.wm.utils.extensions.UIntegers.getLower32
import madisonbay.wm.utils.extensions.ExtLong.Implicits
import madisonbay.csr.all._
import madisonbay.wm.switchwm.ppe.parser.defs.ParserKeys
import madisonbay.wm.switchwm.ppe.parser.defs.ParserKeys.ParserKey

object KeysExtractor {

  type ExtractionIndex = Int

  def extractKeys(csrParser: CsrParser, packetHeader: PacketHeader, allProtocolsOffsets: ProtocolsOffsets, extractionIndex: ExtractionIndex):
      (PacketFields, CsrParser) = {

    val extractorCsr = csrParser.ppeParserMap.PARSER_EXTRACT_CFG(extractionIndex).PARSER_EXTRACT_CFG
    val countersL  = mby_ppe_parser_map._PARSER_COUNTERS
    val ext_unknown_protid = countersL composeLens parser_counters_r._EXT_UNKNOWN_PROTID composeLens parser_counters_r.EXT_UNKNOWN_PROTID._value
    val ext_dup_protid = countersL composeLens parser_counters_r._EXT_DUP_PROTID composeLens parser_counters_r.EXT_DUP_PROTID._value

    def modify(actualState: (Map[ParserKey, Short], mby_ppe_parser_map, Int), parserExtractCfgReg: parser_extract_cfg_r) = {
      val (parserKeys, mbyPpeParserMap, counter) = actualState

      val protocolId = parserExtractCfgReg.PROTOCOL_ID().toInt
      val protocolOffsets = allProtocolsOffsets.protocolPointers(protocolId).map(_.offset)

      def toWordWithOffset(v: Int): Short = packetHeader.getWordSafe(v + getLower32(parserExtractCfgReg.OFFSET()).toInt)

      (protocolId, protocolOffsets) match {

        case (ExtractAction.SpecialProtocolId, _) => (parserKeys, mbyPpeParserMap, counter + 1)

        case (_, Nil) =>
          val next = ext_unknown_protid.modify((v: Long) => v.incWithUByteSaturation)
          (parserKeys, next(mbyPpeParserMap), counter + 1)

        case (_, h :: Nil) =>
          (parserKeys.updated(ParserKeys.getConstant(counter), toWordWithOffset(h)), mbyPpeParserMap, counter + 1)

        case (_, h :: _) =>
          val next = ext_dup_protid.modify((v: Long) => v.incWithUByteSaturation)
          (parserKeys.updated(ParserKeys.getConstant(counter), toWordWithOffset(h)), next(mbyPpeParserMap), counter + 1)

      }
    }

    val (result, updatedParserMap, _) = extractorCsr.foldLeft((Map[ParserKey, Short](), csrParser.ppeParserMap, 0))(modify)

    (PacketFields(result), csrParser.copy(ppeParserMap = updatedParserMap))
  }

}
