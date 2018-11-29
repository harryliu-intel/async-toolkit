
package madisonbay.wm.switchwm.ppe.parser.output

import madisonbay.wm.switchwm.ppe.parser.defs.ParserKeys
import madisonbay.wm.switchwm.ppe.parser.defs.ParserKeys.ParserKey
import madisonbay.wm.utils.extensions.UIntegers._


class PacketFields(val fields: Map[ParserKey, Short]) {

  // 80 fields, of 2 bytes each
  def populateField(startField: ParserKey, contents: Seq[Short]): PacketFields = {
    // obviously need to support field of size > 1!
    val newFields = contents.indices.map(index => (ParserKeys.getConstant(startField.index + index), contents(index)))
    new PacketFields(fields ++ newFields)
  }

  def key8(i: ParserKey): Byte = {
    require((0 until 32).contains(i.index))
    val fval = fields(ParserKeys.getConstant(64 + (i.index / 2)))
    if (i.index % 2 == 1) { getLower8(fval >> 8).toByte }
    else { getLower8(fval).toByte }
  }

  def key16(i: ParserKey): Short = {
    require((0 until 32).contains(i.index))
    fields(i)
  }

  def key16Updated(i: ParserKey, x: Short): PacketFields = new PacketFields(fields.updated(i, x))

  def key32(i: ParserKey): Int = {
    require((0 until 16).contains(i))
    (fields(ParserKeys.getConstant(i.index + 1)).toInt << 16) & fields(i).toInt
  }

  override def toString: String = fields.toList.sortBy { case (key, _) => key }.map {
      case (idx, value) => (idx, getLower16(value).toHexString)
    }.toString

}

object PacketFields {

  def apply(): PacketFields = new PacketFields(Map[ParserKey, Short]())

  def apply(fields: Map[ParserKey, Short]): PacketFields = new PacketFields(fields)

}
