//scalastyle:off regex.tuples
//scalastyle:off
package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser

import com.intel.cg.hpfd.csr.generated._
import ParserTcam._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.PipelineStage
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.mapper.PacketFields
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.Parser._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output.{PacketFlags, ParserOutput}
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.ppe.PortIndex
import com.intel.cg.hpfd.madisonbay.wm.switchwm.util.{IPVersion, Packet, PacketHeader}
import scalaz.State
import scalaz.syntax.traverse._
import scalaz.std.list._

class Parser(csr: mby_ppe_parser_map.mby_ppe_parser_map) extends PipelineStage[Packet, ParserOutput] {

  val stages: IndexedSeq[ParserStage] = (0 until parserStages).map(i => new ParserStage(csr, i))

  def initialState(ph: PacketHeader, port: PortIndex): Parser.ParserState = {
    val portCfg = csr.PARSER_PORT_CFG(port.p)
    val initWOffsets = List(portCfg.INITIAL_W0_OFFSET, portCfg.INITIAL_W1_OFFSET, portCfg.INITIAL_W2_OFFSET)
    val w = initWOffsets.map(off => ph.getWord(off().toInt))
    val aluOp = AluOperation(portCfg.INITIAL_OP_ROT().toShort, portCfg.INITIAL_OP_MASK().toShort)
    val state = portCfg.INITIAL_STATE().toShort
    val ptr = portCfg.INITIAL_PTR().toShort
    ParserState(w,aluOp, state, ptr)
  }

  def packetType(pf: PacketFlags): (PacketType, ExtractionIndex) = {
    val interface = 0
    val tcamCsr = csr.PARSER_PTYPE_TCAM(interface).PARSER_PTYPE_TCAM
    val sramCsr = csr.PARSER_PTYPE_RAM(interface).PARSER_PTYPE_RAM

    val tc = tcamMatch.curried(standardTcamMatchBit)
    tcamCsr.zip(sramCsr).reverse.collectFirst{
      case (x,y) if tc(x.KEY_INVERT, x.KEY, pf.toLong) => (y.PTYPE().toInt, y.EXTRACT_IDX().toInt)
    }.getOrElse((0,0))
  }

  def incrementWithSaturation(saturation: Long): Long => Long = l => math.min(l + 1, saturation)

  //scalastyle:off magic.number
  def extractKeys(packetHeader: PacketHeader, protoOffsets: ProtoOffsets): PacketFields = {
    val ph = packetHeader
    val fieldProfile = 0
    val extractorCsr = csr.PARSER_EXTRACT_CFG(fieldProfile).PARSER_EXTRACT_CFG
    val countersL  = mby_ppe_parser_map.mby_ppe_parser_map._PARSER_COUNTERS

    def modify(parserExtractCfgReg: parser_extract_cfg_r.parser_extract_cfg_r): State[(List[Short], mby_ppe_parser_map.mby_ppe_parser_map), Unit] =
      State(actualState => {
        val (result, mbyPpeParserMap) = actualState
        val protocolId = parserExtractCfgReg.PROTOCOL_ID()
        def toWordWithOffset(v: Int): Short = ph.getWord(v + parserExtractCfgReg.OFFSET().toInt)

        (protocolId, protoOffsets.collect { case (pId, baseOffset) if pId == protocolId => baseOffset }.toList) match {
          case (0xFF, _) => ((0.toShort :: result, mbyPpeParserMap), ())
          case (_, Nil) =>
            val next = countersL.modify(_.EXT_UNKNOWN_PROTID.modify(incrementWithSaturation(255)))
            ((0.toShort :: result, next(mbyPpeParserMap)), ())
          case (_, h :: Nil) =>
            ((toWordWithOffset(h) :: result, mbyPpeParserMap),())
          case (_, h :: _) =>
            val next = countersL.modify(_.EXT_DUP_PROTID.modify(incrementWithSaturation(255)))
            ((toWordWithOffset(h) :: result, next(mbyPpeParserMap)), ())
        }
      })

    val ((result, _), _) = extractorCsr.toList.traverseS(modify)((List.empty, csr))

    // TODO: handle the state here
    PacketFields(result.reverse.toIndexedSeq)
  }
  //scalastyle:on

  val process: Packet => ParserOutput = packet => {

    // TODO: support split header to Interface 0 and Interface 1
    val packetHeader = PacketHeader(packet.bytes.slice(0, PacketHeader.portionSegmentFPP))

    // setup the initial state
    val rxPort = new PortIndex(0) // need to handle this via the function interface somehow...
    val initStage = (initialState(packetHeader, rxPort),
                    PacketFlags(),
                    Parser.EmptyProtoOffsets,
                    Option.empty[ParserException])

    // apply all the parser stages, the packetheader is the same, but pass the other state down the line
    // this needs cleanup!
    val stagesResult  = stages.foldLeft(initStage) {
      (previous, stage) => stage(packetHeader, previous._1, previous._2, previous._3, previous._4)
    }

    val (_, packetFlags, protoOffsets, parserException) = stagesResult

    val paKeysVal = extractKeys(packetHeader, protoOffsets)
    val paPacketTypeVal = packetType(packetFlags)
    // now we have the flags and the proto-offsets
    // the metadata is the flags + a conversion of the packetheader, proto-offsets, and proto-offset configuration into a field vector

    val exceptionStage = parserException match {
      case Some(_) => parserException.get.stageEncountered
      case _ =>
        assert(assertion = false, "No exception encountered in parse, likely buggy parser image")
        0

    }
    val (depthExceeded, headerTruncated, parsingDone) = parserException match {
      case Some(ParserDoneException(_)) => (false, false, true)
      case Some(TruncatedHeaderException(_)) => (false, true, false)
      case Some(ParseDepthExceededException(_)) => (true, false, false)
      case _ => (false,false,false)
    }

    ParserOutput(
      rxPort = rxPort,
      pktMeta = 0,
      rxFlags = 0,
      segMetaErr= false,
      paAdjSegLegLen = 0,
      paKeys = paKeysVal,
      paFlags = packetFlags,
      paPointers = protoOffsets,
      paKeysValid = false,
      paPointersValid = false,
      paCsumOk = false,
      paExceptionStage = exceptionStage,
      paExceptionDepthExceeded = depthExceeded,
      paExceptionTruncHeader = headerTruncated,
      paExParsingDone = parsingDone,
      paDrop = false,
      paPacketType = paPacketTypeVal._1 // (what to do with the extract index)?
    )
  }
}

object Parser {

  case class ParserState(w: List[Short], op: AluOperation, state: Short, ptr: Short)

  type ProtoId = Int
  type BaseOffset = Int
  type ProtoOffsets = IndexedSeq[(ProtoId, BaseOffset)]
  type PacketType = Int
  type ExtractionIndex = Int

  val EmptyProtoOffsets: ProtoOffsets = Vector[(ProtoId, BaseOffset)]((0,0))

  val parserStages = 32

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

  class ParserException(val stageEncountered: Int)

  object ParserException {

    def apply(eos: Boolean = false, eop: Boolean = false, done: Boolean = false, stageEncountered: Int): Parser.ParserException = {
      if (done) {
        ParserDoneException(stageEncountered)
      } else if (eos & eop) {
        TruncatedHeaderException(stageEncountered)
      } else {
        ParseDepthExceededException(stageEncountered)
      }
    }

  }

  class AbortParserException(stageEncountered: Int) extends ParserException(stageEncountered)

  case class TruncatedHeaderException(override val stageEncountered: Int) extends AbortParserException(stageEncountered)

  case class ParseDepthExceededException(override val stageEncountered: Int) extends AbortParserException(stageEncountered)

  case class ParserDoneException(override val stageEncountered: Int) extends ParserException(stageEncountered)

}

