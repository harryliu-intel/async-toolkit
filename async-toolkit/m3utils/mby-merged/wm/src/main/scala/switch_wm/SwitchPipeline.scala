package switch_wm

import com.intel.cg.hpfd.csr.generated.{mby_ppe_mapper_map, mby_ppe_rx_top_map, mby_top_map}

class PacketHeader(val bytes : Array[Byte]) {
  val adjustedSegmentLength = 100 // (should be computed)
  def apply(addr : Int) : Byte = bytes(addr)
  def getWord(addr : Int) : Short = ((apply(addr) << 8) | apply(addr)).toShort
}
object PacketHeader {
  def apply( bytes : Array[Byte]) : PacketHeader = new PacketHeader(bytes)
}

class Metadata(val flags : ppe.PacketFlags, val fields : ppe.PacketFields)
object Metadata {
  def apply(flags : ppe.PacketFlags, fields : ppe.PacketFields): Metadata = new Metadata(flags, fields)
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

class RxPpe(csr : mby_ppe_rx_top_map) extends PipelineStage[Array[Byte], ppe.ParserOutput] {
  val epl = new Epl
  val headerExtractor = new HeaderExtraction
  val parser = new switch_wm.ppe.Parser(csr.parser)
  // val mapper = new KeyMapper(csr.mapper)

  // how do we provide the 'port' here?
  val x : (Array[Byte]) => ppe.ParserOutput = epl.x andThen headerExtractor.x andThen parser.x

}

object SwitchTest {
  val junk = Array.ofDim[Byte](1024)
  val theCsrs = mby_top_map()
  val swp = new RxPpe(theCsrs.mpt(0).rx_ppe)
  swp.x(junk)
}