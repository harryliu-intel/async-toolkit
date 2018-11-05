
package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.mapper

import com.intel.cg.hpfd.madisonbay.wm.switchwm.epl.MACAddress
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.mapper.PacketFields.MACMapperImposed


class PacketFields private (fields: Map[Int, Short]) {

  // 80 fields, of 2 bytes each
  def populateField(startField: Int, contents: Seq[Short]): PacketFields = {
    // obviously need to support field of size > 1!
    val newFields = contents.indices.view.map(index => (startField + index, contents(index)))
    new PacketFields(fields ++ newFields)
  }

  def key8(i: Int): Byte = {
    require((0 until 32).contains(i))
    val fval = fields(64 + (i / 2))
    if (i % 2 == 1) { (fval >> 8 & 0xff).toByte }
    else { (fval & 0xff).toByte }
  }

  def key16(i: Int): Short = {
    require((0 until 32).contains(i))
    fields(i)
  }

  def key16Updated(i: Int, x: Short): PacketFields = {
    new PacketFields(fields.updated(i, x))
  }

  def key32(i: Int): Int = {
    require((0 until 16).contains(i))
    (fields(i + 1).toInt << 16) & fields(i).toInt
  }

  // treat the packet fields, assuming that the parser was configured
  // as expected by the _mapper_
  def assumeIPv4Parsed: PacketFields with MACMapperImposed = new PacketFields(fields) with MACMapperImposed

  override def toString: String = fields.view.toList.sortBy{case (key, _) => key}.map{case (idx, value) => (idx, value & 0xffff)}.toString
}

object PacketFields {

  //scalastyle:off
  // TODO: find dependency between 80 from HeaderExtraction and here
  def apply(): PacketFields = new PacketFields(Map[Int, Short]())

  def apply(fields: Map[Int, Short]): PacketFields = new PacketFields(fields)

  trait MACMapperImposed {
    this: PacketFields =>

    def InnerDMAC: MACAddress = MACAddress(key16(0), key16(1), key16(2))
    def InnerSMAC: MACAddress = MACAddress(key16(3), key16(4), key16(5))
    def OuterDMAC: MACAddress = MACAddress(key16(6), key16(7), key16(8))
    def OuterSMAC: MACAddress = MACAddress(key16(9), key16(10), key16(11))

  }

}


