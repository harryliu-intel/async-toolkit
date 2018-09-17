package com.intel.cg.hpfd.madisonbay.wm.switchwm

import com.intel.cg.hpfd.csr.generated.mby_ppe_rx_top_map
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe._
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.ppe._
import com.intel.cg.hpfd.madisonbay.wm.switch_wm.Packet
import com.intel.cg.hpfd.madisonbay.wm.switch_wm._


class PacketHeader(val bytes : IndexedSeq[Byte]) {
  val maxSegmentSize = 192
  val adjustedSegmentLength = {
    if (bytes.length < 4) bytes.length
    else if ((bytes.length - 4) > maxSegmentSize) maxSegmentSize
    else bytes.length - 4
  }

  /**
    * The packetheader  includes the entire packet
    *
    * Used in parser to help distinguish between exceeding parser depth and
    * exceeding packet size
    */
  val eop = (bytes.length - 4) <= maxSegmentSize

  def apply(addr : Int) : Byte = bytes(addr)
  def getWord(addr : Int) : Short = ((apply(addr + 1) << 8) | apply(addr)).toShort

  def ipVersion : IPVersion.Value = {
    bytes(0).nib(1) match {
      case 0x4 => IPVersion.V4
      case 0x6 => IPVersion.V6
    }
  }
  def totalLength : Int = {
    getWord(2)
  }

}
object PacketHeader {
  def apply( bytes : Array[Byte]) : PacketHeader = new PacketHeader(bytes)
}

class Metadata(val flags : PacketFlags, val fields : PacketFields)
object Metadata {
  def apply(flags : PacketFlags, fields : PacketFields): Metadata = new Metadata(flags, fields)
}

abstract class PipelineStage[I,O] {
  val x : I => O
}

class Epl extends PipelineStage[Array[Byte], Packet] {
  val x : (Array[Byte]) => Packet = (bits) => {
    new Packet(bits)
  }
}

class HeaderExtraction extends PipelineStage[Packet, PacketHeader] {
  val x : (Packet) => PacketHeader = (p) => {
    new PacketHeader (p.bytes.slice(0,80))
  }
}

/*
class KeyMapper(csr : mby_ppe_mapper_map) extends PipelineStage[Metadata, Metadata] {
  val x : (Metadata) => Metadata = (md) => {
    md
  }
}
*/

class RxPpe(csr : mby_ppe_rx_top_map) extends PipelineStage[Array[Byte], ParserOutput] {
  val epl = new Epl
  val headerExtractor = new HeaderExtraction
  val parser = new Parser(csr.parser)
  // val mapper = new KeyMapper(csr.mapper)

  // how do we provide the 'port' here?
  val x : (Array[Byte]) => ParserOutput = epl.x andThen parser.x

}
