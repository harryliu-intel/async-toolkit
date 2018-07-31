package switch_wm



class Packet
class PacketHeader {
  val adjustedSegmentLength = 100 // (should be computed)
  def apply(addr : Int) : Byte = 0
}

class Metadata(val flags : ppe.PacketFlags, val fields : ppe.PacketFields)
object Metadata {
  def apply(flags : ppe.PacketFlags, fields : ppe.PacketFields): Metadata = new Metadata(flags, fields)
}

abstract class PipelineStage[I,O] {
  val x : (I) => O

}

class Epl extends PipelineStage[Array[Byte], Packet] {
  val x : (Array[Byte]) => Packet = (bits) => {
    new Packet
  }
}

class HeaderExtraction extends PipelineStage[Packet, PacketHeader] {
  val x : (Packet) => PacketHeader = (p) => {
    new PacketHeader
  }
}

class KeyMapper(csr : switch_wm.csr.mby_ppe_mapper_map) extends PipelineStage[Metadata, Metadata] {
  val x : (Metadata) => Metadata = (md) => {
    md
  }
}

class RxPpe(csr : switch_wm.csr.mby_ppe_rx_top_map) extends PipelineStage[Array[Byte], Metadata] {
  val epl = new Epl
  val headerExtractor = new HeaderExtraction
  val parser = new switch_wm.ppe.Parser(csr.parser)
  val mapper = new KeyMapper(csr.mapper)

  // how do we provide the 'port' here?
  val x : (Array[Byte]) => Metadata = epl.x andThen headerExtractor.x andThen parser.x andThen mapper.x

}

object SwitchTest {
  val junk = Array.ofDim[Byte](1024)
  val theCsrs = csr.mby_top_map()
  val swp = new RxPpe(theCsrs.mpt(0).rx_ppe)
  swp.x(junk)
}