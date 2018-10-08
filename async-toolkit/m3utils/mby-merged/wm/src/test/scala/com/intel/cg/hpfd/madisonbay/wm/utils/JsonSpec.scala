package com.intel.cg.hpfd.madisonbay.wm.utils

import org.scalatest.{FlatSpec, Matchers}

//scalastyle:off
class JsonSpec extends FlatSpec with Matchers {

  val input: String =
    """
      |{
      |      "name" : "scalars_0",
      |      "id" : 0,
      |      "fields" : [
      |        ["Usr_Meta.src_port", 9, false],
      |        ["Usr_Meta.drop", 1, false],
      |        ["Usr_Meta.padding", 22, false]
      |      ]
      |    }
    """.stripMargin

  "Json" should "parse string" in {

    Json.parse(input).get("name") shouldEqual Some("scalars_0")

  }

}
