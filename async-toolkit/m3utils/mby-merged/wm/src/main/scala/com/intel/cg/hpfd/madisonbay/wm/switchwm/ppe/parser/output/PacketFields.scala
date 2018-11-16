
package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output

import com.intel.cg.hpfd.madisonbay.wm.utils.extensions.UIntegers._


class PacketFields(val fields: Map[Int, Short]) {

  // 80 fields, of 2 bytes each
  def populateField(startField: Int, contents: Seq[Short]): PacketFields = {
    // obviously need to support field of size > 1!
    val newFields = contents.indices.map(index => (startField + index, contents(index)))
    new PacketFields(fields ++ newFields)
  }

  def key8(i: Int): Byte = {
    require((0 until 32).contains(i))
    val fval = fields(64 + (i / 2))
    if (i % 2 == 1) { getLower8(fval >> 8).toByte }
    else { getLower8(fval).toByte }
  }

  def key16(i: Int): Short = {
    require((0 until 32).contains(i))
    fields(i)
  }

  def key16Updated(i: Int, x: Short): PacketFields = new PacketFields(fields.updated(i, x))

  def key32(i: Int): Int = {
    require((0 until 16).contains(i))
    (fields(i + 1).toInt << 16) & fields(i).toInt
  }

  override def toString: String = fields.toList.sortBy { case (key, _) => key }.map {
      case (idx, value) => (idx, getLower16(value))
    }.toString

}

object PacketFields {

  def apply(): PacketFields = new PacketFields(Map[Int, Short]())

  def apply(fields: Map[Int, Short]): PacketFields = new PacketFields(fields)

}
