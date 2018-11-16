//scalastyle:off
package com.intel.cg.hpfd.madisonbay.wm.utils.json

import com.intel.cg.hpfd.madisonbay.wm.utils.Loader
import com.intel.cg.hpfd.madisonbay.wm.utils.json.JsonReader._
import org.scalatest.{FlatSpec, Matchers}

class JsonReaderSpec extends FlatSpec with Matchers {

  "Json id of element in nested lists" should "match" in {
    "abc(3)(5)(8)".matches(JsonReader.PatternListApplyElement) shouldEqual true
  }

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
    JsonReader.parse(input).get.get("name") shouldEqual Some("scalars_0")

    val json: Map[String, Any] = Map(
      "parsers" -> List(
        Map("p1" -> List(1,2,3)),
        Map("p2" -> List(4,5,6))
      ),
      "headers" -> List(
        List("h1", 1, false, Map("h12" -> Map("a"->0))),
        List("h2", 2, true,  Map("h22" -> Map("b"->1)))
      )
    )

    getOpt(json, "parsers(1).p2")    shouldEqual Some(List(4,5,6))
    getListOpt(json, "parsers(1).p2")    shouldEqual Some(List(4,5,6))
    getOpt(json, "parsers(1).p2(2)") shouldEqual Some(6)
    getIntOpt(json, "parsers(1).p2(2)") shouldEqual Some(6)
    getMapOpt(json, "parsers(1).p2(2)") shouldEqual None
    getListOpt(json, "parsers(1).p2(2)") shouldEqual None
    getOpt(json, "headers(0)(0)")    shouldEqual Some("h1")
    getStringOpt(json, "headers(0)(0)")    shouldEqual Some("h1")
    getOpt(json, "headers(0)(3).h12")    shouldEqual Some(Map("a"->0))
    getMapOpt(json, "headers(0)(3).h12")    shouldEqual Some(Map("a"->0))
    getOpt(json, "headers(0)(3).h12.a")    shouldEqual Some(0)

    getOpt(json, "parsers(1).p2a")    shouldEqual None
    getOpt(json, "parsers(-1).p2a")   shouldEqual None
    getOpt(json, "parsers((1).p2a")   shouldEqual None
    getOpt(json, "parsers.(1)")       shouldEqual None
    getOpt(json, "headers(0)(2)(1)")    shouldEqual None
    getOpt(json, "headers(0)(3).h13")    shouldEqual None
    getOpt(json, "headers(0)(3).h12.a(0)")    shouldEqual None
  }

  it should "get paths from json" in {
    val inputJson = parse(input)
    getOpt(inputJson.get, "fields(1)(2)") shouldEqual Some(false)
    getBooleanOpt(inputJson.get, "fields(1)(2)") shouldEqual Some(false)
  }

  it should "properly load json and extract fields" in {
    val json = Loader.loadJson("src/test/resources/json/p4_output_test.json").getOrElse(Map())
    json should not equal Map()

    getDoubleOpt(json, "double_test") shouldEqual Some(0.5)
    getIntOpt(json, "double_test") shouldEqual None
    getOpt(json, "header_types(1).fields(0)(1)") shouldEqual Some(9)
    getIntOpt(json, "header_types(1).fields(0)(1)") shouldEqual Some(9)
    getOpt(json, "headers(2).name") shouldEqual Some("ethernet")
    getStringOpt(json, "headers(2).name") shouldEqual Some("ethernet")
    getOpt(json, "parsers(0).parse_states(0).parser_ops(0).parameters(0).type") shouldEqual Some("regular")
  }

  it should "read big ints" in {
    val bigInteger = "1234567890987654321212121232245354353465654645"
    val m = parse(" { \"number\": " + bigInteger + " } ").get
    m.getAnyInt("number") shouldEqual BigInt(bigInteger)
  }

  it should "read big decimals" in {
    val bigDecimal = "1234567890987654321212121232245354353465654645.5"
    val m = parse(" { \"number\": " + bigDecimal + " } ").get
    m.getAnyDecimal("number") shouldBe a [BigDecimal]
  }

}
