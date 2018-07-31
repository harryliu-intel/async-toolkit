package switch_wm.ppe

class PacketFields(val fields : Vector[Short]) {
  // 80 fields, of 2 bytes each
  def populateField(startField : Int, contents : Seq[Short]) : PacketFields = {
    // obviously need to support field of size > 1!
    new PacketFields(fields updated(startField, contents.head))
  }
}

object PacketFields{
  def apply() : PacketFields = new PacketFields(Vector.fill[Short](80)(0.toShort))
}