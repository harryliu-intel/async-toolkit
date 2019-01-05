//scalastyle:off
package madisonbay.wm.switchwm.parser

import madisonbay.wm.switchwm.ppe.parser.defs.ParserKeys
import madisonbay.wm.switchwm.ppe.parser.output.PacketFields
import org.scalatest.{FlatSpec, Matchers}

class PacketFieldsSpec extends FlatSpec with Matchers {

  "PacketFields" should "correctly populate field from sequence" in {
    val fields = PacketFields().populateField(ParserKeys.getConstant(4), List[Short](1, 2, 3))
    fields.key16(ParserKeys.getConstant(4)) shouldEqual 1
    fields.key16(ParserKeys.getConstant(5)) shouldEqual 2
    fields.key16(ParserKeys.getConstant(6)) shouldEqual 3
  }

}
