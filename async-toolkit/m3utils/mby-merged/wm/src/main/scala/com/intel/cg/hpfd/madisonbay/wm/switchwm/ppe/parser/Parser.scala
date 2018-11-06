//scalastyle:off regex.tuples
package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser

import madisonbay.csr.all._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output.ParserExceptions._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.csr.Csr.CsrParser
import com.intel.cg.hpfd.madisonbay.wm.switchwm.epl.{IPVersion, Packet, PacketHeader}
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output.{PacketFields, ParserOutput}
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.ppe.Port
import com.intel.cg.hpfd.madisonbay.wm.utils.extensions.ExtLong.Implicits
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.internal._
import com.intel.cg.hpfd.madisonbay.wm.utils.BitFlags
import com.intel.cg.hpfd.madisonbay.wm.utils.extensions.UIntegers._

import scala.annotation.tailrec

object Parser {

  case class ParserState(w: Array[Short], aluOperation: AluOperation, state: Short, ptr: Short) {
    override def toString: String = s"ParserState(${w.toList.map(e => f"0x$e%X")},$aluOperation,state=$state,ptr=$ptr)"
  }

  case class HeaderPointer(protocolId: ProtoId, offset: BaseOffset)

  type ProtoId          = Int
  type BaseOffset       = Int
  type PacketType       = Int
  type ExtractionIndex  = Int
  type PointerNumber    = Int
  type ProtoOffsets     = Map[PointerNumber, HeaderPointer]

  val NumberOfParsingStages = 32

  def parse(csrParser: CsrParser, packet: Packet, rxPort: Port): ParserOutput = {

    // TODO: support split header to Interface 0 and Interface 1
    val packetHeader = PacketHeader(packet).trimmed

    val (packetFlags, protoOffsets, parserExceptionOpt) = applyActions(csrParser, packetHeader, rxPort)

    val (packetType, extractionIndex) = getPacketType(csrParser, packetFlags)

    val (paKeysVal, csrExtractedKeys) = extractKeys(csrParser, packetHeader, protoOffsets, extractionIndex)

    // now we have the flags and the proto-offsets
    // the metadata is the flags + a conversion of the packetheader, proto-offsets, and proto-offset configuration into a field vector
    ParserOutput(
      updatedParserCsr          = csrExtractedKeys,
      rxPort                    = rxPort,
      pktMeta                   = 0,
      rxFlags                   = 0,
      segMetaErr                = false,
      paAdjSegLegLen            = 0,
      parserKeys                    = paKeysVal,
      parserFlags                   = packetFlags,
      parserPointers                = protoOffsets,
      parserKeysValid               = false,
      parserPointersValid           = false,
      parserCsumOk                  = false,
      parserException          = parserExceptionOpt,
      drop                    = false,
      packetType              = packetType
    )
  }

  def applyActions(csr: CsrParser, packetHeader: PacketHeader, rxPort: Port): (BitFlags, ProtoOffsets, Option[ParserException]) =
    applyStage(csr, packetHeader)(0, initialState(csr, packetHeader, rxPort),
      BitFlags(), Map[PointerNumber, HeaderPointer](), Option.empty[ParserException])

  @tailrec
  private def applyStage(csr: CsrParser, packetHeader: PacketHeader)
       (idStage: Int, parserState: ParserState, packetFlags: BitFlags, fields: ProtoOffsets, exceptionOpt: Option[ParserException]):
                        (BitFlags, ProtoOffsets, Option[ParserException]) = idStage match {

    case NumberOfParsingStages => (packetFlags, fields, exceptionOpt)

    case stage =>
      val action = matchingAction(csr, stage, parserState)
      (exceptionOpt, action) match {

        case (Some(exc), _) => (packetFlags, fields, Some(exc))

            // if nothing matches, do nothing
        case (exOpt, None)  =>
          applyStage(csr, packetHeader)(idStage + 1, parserState, packetFlags, fields, exOpt)

        case (_, Some(act)) =>                                    // otherwise, apply the action
          val (actParsState, actPckFlags, actProtOffs, actPrsExcOpt) = act.run(idStage, parserState, packetFlags, fields)(packetHeader)
          applyStage(csr, packetHeader)(idStage + 1, actParsState, actPckFlags, actProtOffs, actPrsExcOpt)
    }
  }

  /**
    * Do the TCAM lookup, translate the result into actions
    */
  private def matchingAction(csrParser: CsrParser, idStage: Int, parserState: ParserState): Option[Action] = {
    // All lists have size of 16
    @tailrec
    def findAction(keysW: List[parser_key_w_r], keysS: List[parser_key_s_r],
                   anaWs: List[parser_ana_w_r], anaSs: List[parser_ana_s_r],
                   exts:  List[parser_ext_r],   excs:  List[parser_exc_r]): Option[Action] =
      (keysW, keysS, anaWs, anaSs, exts, excs) match {
        case (kW :: _, kS :: _, aW :: _, aS :: _, _ :: _, ec :: _) if ParserTcam.camMatching(kW, kS, parserState) =>
                Some(new Action(
                  AnalyzerAction(aW, aS),
                  ExtractAction(exts),
                  ExceptionAction(ec)
                ))

        case (_ :: kWs, _ :: kSs, _ :: aWs, _ :: aSs, _ :: exs, _ :: ecs) => findAction(kWs, kSs, aWs, aSs, exs, ecs)

        case (_, _, _, _, _, _) => None
      }

    val parserMap = csrParser.ppeParserMap
    // reverse because: when multiple rules hit, the highest numbered rule within the stage wins
    findAction(parserMap.PARSER_KEY_W(idStage).PARSER_KEY_W.reverse, parserMap.PARSER_KEY_S(idStage).PARSER_KEY_S.reverse,
      parserMap.PARSER_ANA_W(idStage).PARSER_ANA_W.reverse, parserMap.PARSER_ANA_S(idStage).PARSER_ANA_S.reverse,
      parserMap.PARSER_EXT(idStage).PARSER_EXT.reverse,     parserMap.PARSER_EXC(idStage).PARSER_EXC.reverse)
  }

  private class Action(analyzerAction: AnalyzerAction, extractActions: List[ExtractAction], exceptionAction: ExceptionAction) {

    def run(idStage: Int, parserState: ParserState, parserFlags: BitFlags, protoOffsets: Parser.ProtoOffsets)
             (packetHeader: PacketHeader): (ParserState, BitFlags, Parser.ProtoOffsets, Option[ParserException]) = {
      val currentOffset = parserState.ptr
      // is there an error condition requiring an abort (i.e. without processing this stage)
      // or is the exception action to do nothing (or mark 'done')
      exceptionAction.test(packetHeader, currentOffset, idStage) match {

        case Some(ape: AbortParserException) => (parserState, parserFlags, protoOffsets, Some(ape))

        case parsExcOpt =>
          val updatedParserState = analyzerAction.analyze(packetHeader, parserState) // setup the analyze actions for the next stage
            // do all of the extraction operations to add more to the flags and offsets
          val (updatedProtoOffsets, updatedPaFlags) = ExtractAction.extractActions(extractActions, protoOffsets, parserFlags)
          (updatedParserState, updatedPaFlags, updatedProtoOffsets, parsExcOpt)
      }
    }

  }

  def initialState(csrParser: CsrParser, packetHeader: PacketHeader, port: Port): Parser.ParserState = {
    val portCfg = csrParser.ppeParserMap.PARSER_PORT_CFG(port.index)
    val w = Array(portCfg.INITIAL_W0_OFFSET(), portCfg.INITIAL_W1_OFFSET(), portCfg.INITIAL_W2_OFFSET()).map(offset =>
      packetHeader.getWord(getLower16(offset))
    )
    val aluOp = AluOperation(portCfg.INITIAL_OP_ROT().toShort, portCfg.INITIAL_OP_MASK().toShort)
    ParserState(w, aluOp, portCfg.INITIAL_STATE().toShort, portCfg.INITIAL_PTR().toShort)
  }

  private def getPacketType(csrParser: CsrParser, packetFlags: BitFlags): (PacketType, ExtractionIndex) = {
    val interface = 0
    val tcamCsr = csrParser.ppeParserMap.PARSER_PTYPE_TCAM(interface).PARSER_PTYPE_TCAM
    val sramCsr = csrParser.ppeParserMap.PARSER_PTYPE_RAM(interface).PARSER_PTYPE_RAM
    val flag = packetFlags.toInt

    tcamCsr.zip(sramCsr).reverse.collectFirst {
      case (x,y) if ParserTcam.matchPtype(x.KEY().toInt, x.KEY_INVERT().toInt, flag) =>
        (getLower32(y.PTYPE()).toInt, getLower32(y.EXTRACT_IDX()).toInt)
    }.getOrElse((0,0))
  }

  private def extractKeys(csrParser: CsrParser, packetHeader: PacketHeader, protoOffsets: ProtoOffsets,
                          extractionIndex: ExtractionIndex): (PacketFields, CsrParser) = {
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

  /**
    * Check Payload Length
    */
  def payloadValidate(packet: Packet, header: PacketHeader, otr_l3_ptr: Int): Boolean = {
    // length check varies based on IPv4 (where the length includes the IP header)
    // versus IPv6 (where the payload length includes all extension headers but not the IP header itself)
    header.ipVersion match {
      case IPVersion.IPV4 => header.totalLength <= (packet.bytes.length - otr_l3_ptr - 4)
      case IPVersion.IPV6 => header.totalLength <= (packet.bytes.length - otr_l3_ptr - 40 - 4)
    }
  }

}

