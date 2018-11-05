//scalastyle:off
package com.intel.cg.hpfd.madisonbay.wm.switchwm.parser

import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.mapper.PacketFields
import org.scalatest.{FlatSpec, Matchers}

class PacketFieldsSpec extends FlatSpec with Matchers {
"PacketFields" should "correctly populate field from sequence" in {
  val fields = PacketFields().populateField(4, List[Short](1, 2, 3))
  fields.key16(4) shouldEqual 1
  fields.key16(5) shouldEqual 2
  fields.key16(6) shouldEqual 3
}
}
