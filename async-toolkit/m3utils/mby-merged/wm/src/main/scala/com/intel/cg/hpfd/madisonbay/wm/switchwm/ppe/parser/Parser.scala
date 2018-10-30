//scalastyle:off regex.tuples
package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser

import madisonbay.csr.all._
import ParserExceptions._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.epl.{IPVersion, Packet, PacketHeader}
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.mapper.PacketFields
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output.{PacketFlags, ParserOutput}
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.ppe.PortIndex
import com.intel.cg.hpfd.madisonbay.wm.switchwm.util.Tcam
import com.intel.cg.hpfd.madisonbay.wm.utils.extensions.ExtLong.Implicits
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.actions.{AnalyzerAction, ExceptionAction, ExtractAction}
import com.intel.cg.hpfd.madisonbay.wm.utils.extensions.UIntegers._
import scalaz.State
import scalaz.syntax.traverse._
import scalaz.std.list._

import scala.annotation.tailrec

object Parser {

  case class ParserState(w: Array[Short], aluOperation: AluOperation, state: Short, ptr: Short) {
    override def toString: String = s"ParserState(${w.toList.map(e => f"0x$e%X")},$aluOperation,state=$state,ptr=$ptr)"
  }

  type ProtoId          = Int
  type BaseOffset       = Int
  type PacketType       = Int
  type ExtractionIndex  = Int
  type ProtoOffsets     = IndexedSeq[(ProtoId, BaseOffset)]

  val EmptyProtoOffsets: ProtoOffsets = Vector[(ProtoId, BaseOffset)]((0,0))

  val NumberOfParsingStages       = 32

  val NumberOfExtractionConfs     = 16
  val OffsetOfNextExtractAction   = 16

  def parse(csrParser: mby_ppe_parser_map, packet: Packet, portIndex: Int): ParserOutput = {

    // TODO: support split header to Interface 0 and Interface 1
    val packetHeader = PacketHeader(packet.bytes).trimmed

    val rxPort = new PortIndex(portIndex)

    val (packetFlags, protoOffsets, parserExceptionOpt) = applyStage(csrParser, packetHeader)(0,
      initialState(csrParser, packetHeader, rxPort),
      PacketFlags(), Parser.EmptyProtoOffsets, Option.empty[ParserException])

    val paPacketTypeVal = packetType(csrParser, packetFlags)

    val (paKeysVal, csrExtractedKeys) = extractKeys(csrParser, packetHeader, protoOffsets)

    // now we have the flags and the proto-offsets
    // the metadata is the flags + a conversion of the packetheader, proto-offsets, and proto-offset configuration into a field vector
    ParserOutput(
      updatedParserCsr          = csrExtractedKeys,
      rxPort                    = rxPort,
      pktMeta                   = 0,
      rxFlags                   = 0,
      segMetaErr                = false,
      paAdjSegLegLen            = 0,
      paKeys                    = paKeysVal,
      paFlags                   = packetFlags,
      paPointers                = protoOffsets,
      paKeysValid               = false,
      paPointersValid           = false,
      paCsumOk                  = false,
      paParseException          = parserExceptionOpt,
      paDrop                    = false,
      paPacketType              = paPacketTypeVal._1 // (what to do with the extract index)?
    )
  }

  @tailrec
  def applyStage(csr: mby_ppe_parser_map, packetHeader: PacketHeader)
       (idStage: Int, parserState: ParserState, packetFlags: PacketFlags, fields: ProtoOffsets, exceptionOpt: Option[ParserException]):
                        (PacketFlags, ProtoOffsets, Option[ParserException]) = idStage match {

    case NumberOfParsingStages => (packetFlags, fields, exceptionOpt)

    case stage =>
      val action = matchingAction(csr, stage, parserState)
      (exceptionOpt, action) match {

        case (Some(exc), _) => (packetFlags, fields, Some(exc))

            // if nothing matches, do nothing
        case (exOpt, None)  =>
          print(s"no matching action in stage $idStage\n")
          applyStage(csr, packetHeader)(idStage + 1, parserState, packetFlags, fields, exOpt)

        case (_, Some(act)) =>                                    // otherwise, apply the action
          val (actParsState, actPckFlags, actProtOffs, actPrsExcOpt) = act.run(idStage, parserState, packetFlags, fields)(packetHeader)
          applyStage(csr, packetHeader)(idStage + 1, actParsState, actPckFlags, actProtOffs, actPrsExcOpt)
    }
  }

  /**
    * Do the TCAM lookup, translate the result into actions
    */
  private def matchingAction(csr: mby_ppe_parser_map, idStage: Int, parserState: ParserState): Option[Action] = {
    // All lists have size of 16
    @tailrec
    def findAction(keysW: List[parser_key_w_r], keysS: List[parser_key_s_r],
                   anaWs: List[parser_ana_w_r], anaSs: List[parser_ana_s_r],
                   exts:  List[parser_ext_r],     excs:  List[parser_exc_r]): Option[Action] =
      (keysW, keysS, anaWs, anaSs, exts, excs) match {
        case (kW :: _, kS :: _, aW :: _, aS :: _, ex :: _, ec :: _) if ParserTcam.camMatching(kW, kS, parserState) =>
          //scalastyle:off
            print(s"found action in stage $idStage rule ${keysS.length-1}\nkey w: (w1 value: ${kW.W1_VALUE()}, w1 mask: ${kW.W1_MASK()}, w0 value: " +
              kW.W0_VALUE() + ", w0 mask: " + kW.W0_MASK() + ")\n")
            print("key s: (state value: " + kS.STATE_VALUE() + ", state mask: " + kS.STATE_MASK() + ")\n")
            print("ana w: (next w0 offset: " + aW.NEXT_W0_OFFSET() + ", next w1 offset: " + aW.NEXT_W1_OFFSET() +
              ", next w2 offset: " + aW.NEXT_W2_OFFSET() + ", skip: " + aW.SKIP() + ")\n")
            print("ana s: (next state: " + aS.NEXT_STATE() + ", next state mask: " + aS.NEXT_STATE_MASK() +
              ", next op: " + aS.NEXT_OP() + ")\n")
            print("exc: (ex offset: " + ec.EX_OFFSET() + ", parsing done: " + ec.PARSING_DONE() + ")\n")
            print("ext: (protocol id: " + ex.PROTOCOL_ID() + ", offset: " + ex.OFFSET() + ", flag num: " + ex.FLAG_NUM() +
              ", flag value: " + ex.FLAG_VALUE() + ", ptr num: " + ex.PTR_NUM() + ")\n")

                Some(new Action(
                  AnalyzerAction(aW, aS),
                  List(ExtractAction(ex), ExtractAction(exts(OffsetOfNextExtractAction))),
                  ExceptionAction(ec)
                ))

        case (_ :: kWs, _ :: kSs, _ :: aWs, _ :: aSs, _ :: exs, _ :: ecs) => findAction(kWs, kSs, aWs, aSs, exs, ecs)

        case (_, _, _, _, _, _) => None
      }

    // reverse because: when multiple rules hit, the highest numbered rule within the stage wins
    findAction(csr.PARSER_KEY_W(idStage).PARSER_KEY_W.reverse, csr.PARSER_KEY_S(idStage).PARSER_KEY_S.reverse,
               csr.PARSER_ANA_W(idStage).PARSER_ANA_W.reverse, csr.PARSER_ANA_S(idStage).PARSER_ANA_S.reverse,
               csr.PARSER_EXT(idStage).PARSER_EXT.reverse,     csr.PARSER_EXC(idStage).PARSER_EXC.reverse)
  }

  private class Action(analyzerAction: AnalyzerAction, extractActions: List[ExtractAction], exceptionAction: ExceptionAction) {

    def run(idStage: Int, parserState: ParserState, parserFlags: PacketFlags, protoOffsets: Parser.ProtoOffsets)
             (packetHeader: PacketHeader): (ParserState, PacketFlags, Parser.ProtoOffsets, Option[ParserException]) = {
      val currentOffset = parserState.ptr
      // is there an error condition requiring an abort (i.e. without processing this stage)
      // or is the exception action to do nothing (or mark 'done')
      exceptionAction.test(packetHeader, currentOffset, idStage) match {

        case Some(ape: AbortParserException) => (parserState, parserFlags, protoOffsets, Some(ape))

        case parsExcOpt =>
          val updatedParserState = analyzerAction.analyze(packetHeader, parserState) // setup the analyze actions for the next stage
            // do all of the extraction operations to add more to the flags and offsets
          val (updatedProtoOffsets, updatedPckFlags) = extractActions.foldLeft(protoOffsets, parserFlags) {
              (prev, act) => act.extract(prev)
            }

          print(s"after actions:\nparser state: $updatedParserState, protoOffsets: $updatedProtoOffsets, pckFlags: $updatedPckFlags\n")

          (updatedParserState, updatedPckFlags, updatedProtoOffsets, parsExcOpt)
      }
    }

  }

  def initialState(csr: mby_ppe_parser_map, packetHeader: PacketHeader, portIndex: PortIndex): Parser.ParserState = {
    val portCfg = csr.PARSER_PORT_CFG(portIndex.p)
    val w = Array(portCfg.INITIAL_W0_OFFSET(), portCfg.INITIAL_W1_OFFSET(), portCfg.INITIAL_W2_OFFSET()).map(offset =>
      packetHeader.getWord(getLower16(offset))
    )
    val aluOp = AluOperation(portCfg.INITIAL_OP_ROT().toShort, portCfg.INITIAL_OP_MASK().toShort)
    val state = portCfg.INITIAL_STATE().toShort
    val ptr = portCfg.INITIAL_PTR().toShort
    ParserState(w, aluOp, state, ptr)
  }

  private def packetType(csr: mby_ppe_parser_map, packetFlags: PacketFlags): (PacketType, ExtractionIndex) = {
    val interface = 0
    val tcamCsr = csr.PARSER_PTYPE_TCAM(interface).PARSER_PTYPE_TCAM
    val sramCsr = csr.PARSER_PTYPE_RAM(interface).PARSER_PTYPE_RAM

    val tc = ParserTcam.matchRegister(Tcam.matchBitFun)(_)
    tcamCsr.zip(sramCsr).reverse.collectFirst {
      case (x,y) if tc(ParserTcam.TcTriple(x.KEY_INVERT, x.KEY, packetFlags.toLong)) =>
        (getLower32(y.PTYPE()).toInt, getLower32(y.EXTRACT_IDX()).toInt)
    }.getOrElse((0,0))
  }

  private def extractKeys(csr: mby_ppe_parser_map, packetHeader: PacketHeader,
                          protoOffsets: ProtoOffsets): (PacketFields, mby_ppe_parser_map) = {
    val fieldProfile = 0
    val extractorCsr = csr.PARSER_EXTRACT_CFG(fieldProfile).PARSER_EXTRACT_CFG
    val countersL  = mby_ppe_parser_map._PARSER_COUNTERS
    val ext_unknown_protid = countersL composeLens parser_counters_r._EXT_UNKNOWN_PROTID composeLens parser_counters_r.EXT_UNKNOWN_PROTID._value
    val ext_dup_protid = countersL composeLens parser_counters_r._EXT_DUP_PROTID composeLens parser_counters_r.EXT_DUP_PROTID._value

    def modify(parserExtractCfgReg: parser_extract_cfg_r): State[(List[Short], mby_ppe_parser_map), Unit] =
      State { actualState =>
        val (result, mbyPpeParserMap) = actualState
        val protocolId = parserExtractCfgReg.PROTOCOL_ID()
        def toWordWithOffset(v: Int): Short = packetHeader.getWord(v + getLower32(parserExtractCfgReg.OFFSET()).toInt)

        (protocolId, protoOffsets.collect { case (pId, baseOffset) if pId == protocolId => baseOffset }.toList) match {

          case (ExtractAction.SpecialProtocolId, _) => ((0.toShort :: result, mbyPpeParserMap), ())

          case (_, Nil) =>
            val next = ext_unknown_protid.modify((v: Long) => v.incWithUByteSaturation)
            ((0.toShort :: result, next(mbyPpeParserMap)), ())

          case (_, h :: Nil) =>
            ((toWordWithOffset(h) :: result, mbyPpeParserMap),())

          case (_, h :: _) =>
            val next = ext_dup_protid.modify((v: Long) => v.incWithUByteSaturation)
            ((toWordWithOffset(h) :: result, next(mbyPpeParserMap)), ())

        }
      }

    val ((result, updatedCsr), _) = extractorCsr.toList.traverseS(modify)((List.empty, csr))

    (PacketFields(result.reverse.toIndexedSeq), updatedCsr)
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

