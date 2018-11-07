//scalastyle:off regex.tuples
package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser

import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output.ParserExceptions._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.csr.Csr.CsrParser
import com.intel.cg.hpfd.madisonbay.wm.switchwm.epl.{IPVersion, Packet, PacketHeader}
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output.ParserOutput
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.ppe.Port
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

    val (packetType, extractionIndex) = PacketType.derive(csrParser, packetFlags)

    val (paKeysVal, csrExtractedKeys) = KeysExtractor.extractKeys(csrParser, packetHeader, protoOffsets, extractionIndex)

    // now we have the flags and the proto-offsets
    // the metadata is the flags + a conversion of the packetheader, proto-offsets, and proto-offset configuration into a field vector
    ParserOutput(
      updatedParserCsr          = csrExtractedKeys,
      rxPort                    = rxPort,
      pktMeta                   = 0,
      rxFlags                   = 0,
      segMetaErr                = false,
      paAdjSegLegLen            = 0,
      parserKeys                = paKeysVal,
      parserFlags               = packetFlags,
      parserPointers            = protoOffsets,
      parserKeysValid           = false,
      parserPointersValid       = false,
      parserCsumOk              = false,
      parserException           = parserExceptionOpt,
      drop                      = false,
      packetType                = packetType
    )
  }

  def applyActions(csr: CsrParser, packetHeader: PacketHeader, rxPort: Port): (BitFlags, ProtoOffsets, Option[ParserException]) =
    applyStage(csr, packetHeader)(0, initialState(csr, packetHeader, rxPort), BitFlags(), Map[PointerNumber,
      HeaderPointer](), Option.empty[ParserException])

  @tailrec
  private def applyStage(csr: CsrParser, packetHeader: PacketHeader)
       (idStage: Int, parserState: ParserState, packetFlags: BitFlags, fields: ProtoOffsets, exceptionOpt: Option[ParserException]):
                        (BitFlags, ProtoOffsets, Option[ParserException]) = idStage match {

    case NumberOfParsingStages => (packetFlags, fields, exceptionOpt)

    case stage =>
      val action = ParserAction.matchingAction(csr, stage, parserState)
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

  def initialState(csrParser: CsrParser, packetHeader: PacketHeader, port: Port): Parser.ParserState = {
    val portCfg = csrParser.ppeParserMap.PARSER_PORT_CFG(port.index)
    val w = Array(portCfg.INITIAL_W0_OFFSET(), portCfg.INITIAL_W1_OFFSET(), portCfg.INITIAL_W2_OFFSET()).map(offset =>
      packetHeader.getWord(getLower16(offset))
    )
    val aluOp = AluOperation(portCfg.INITIAL_OP_ROT().toShort, portCfg.INITIAL_OP_MASK().toShort)
    ParserState(w, aluOp, portCfg.INITIAL_STATE().toShort, portCfg.INITIAL_PTR().toShort)
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

