//scalastyle:off
package madisonbay.wm.utils.json

import madisonbay.wm.utils.FileService
import madisonbay.wm.utils.json.JsonReader._
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.OptionValues._

class JsonReaderSpec extends FlatSpec with Matchers {

  "Json id of element in nested lists" should "match" in {
    "abc(3)(5)(8)" should fullyMatch regex JsonReader.PatternListApplyElement
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
    JsonReader.parse(input).get.get("name").value should equal ("scalars_0")

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

    getOpt(json, "parsers(1).p2").value should equal (List(4,5,6))
    getOptFromUri(json, "parsers/1/p2").value should equal (List(4,5,6))

    getListOpt(json, "parsers(1).p2").value should equal (List(4,5,6))
    getOpt(json, "parsers(1).p2(2)").value should equal (6)
    getIntOpt(json, "parsers(1).p2(2)").value should equal (6)
    getMapOpt(json, "parsers(1).p2(2)") should be (empty)
    getListOpt(json, "parsers(1).p2(2)") should be (empty)
    getOpt(json, "headers(0)(0)").value should equal ("h1")
    getStringOpt(json, "headers(0)(0)").value should equal ("h1")
    getOpt(json, "headers(0)(3).h12").value should equal (Map("a"->0))
    getMapOpt(json, "headers(0)(3).h12").value should equal (Map("a"->0))

    getOpt(json, "headers(0)(3).h12.a").value should equal (0)
    getOptFromUri(json, "headers/0/3/h12/a").value should equal (0)

    getOpt(json, "parsers(1).p2a") should be (empty)
    getOptFromUri(json, "parsers/1/p2a") should be (empty)

    getOpt(json, "parsers(-1).p2a") should be (empty)
    getOpt(json, "parsers((1).p2a") should be (empty)
    getOptFromUri(json, "parsers//1/p2a") should be (empty)

    getOpt(json, "parsers.(1)") should be (empty)
    getOpt(json, "headers(0)(2)(1)") should be (empty)
    getOptFromUri(json, "headers/0/2/1") should be (empty)

    getOpt(json, "headers(0)(3).h13") should be (empty)
    getOpt(json, "headers(0)(3).h12.a(0)") should be (empty)
    getOptFromUri(json, "headers/0/3/h12/a/0") should be (empty)
  }

  it should "get paths from json" in {
    val inputJson = parse(input)
    getOpt(inputJson.get, "fields(1)(2)").value should equal (false)
    getBooleanOpt(inputJson.get, "fields(1)(2)").value should equal (false)
  }

  it should "properly load json and extract fields" in {
    val json = FileService.loadJson("src/test/resources/json/p4_output_test.json").getOrElse(Map())
    json should not be empty

    getDoubleOpt(json, "double_test").value should equal (0.5)
    getIntOpt(json, "double_test") should be (empty)
    getOpt(json, "header_types(1).fields(0)(1)").value should equal (9)
    getIntOpt(json, "header_types(1).fields(0)(1)").value should equal (9)
    getOpt(json, "headers(2).name").value should equal ("ethernet")
    getStringOpt(json, "headers(2).name").value should equal ("ethernet")
    getOpt(json, "parsers(0).parse_states(0).parser_ops(0).parameters(0).type").value should equal ("regular")
    getOptFromUri(json, "parsers/0/parse_states/0/parser_ops/0/parameters/0/type").value should equal ("regular")
  }

  it should "read big ints" in {
    val bigInteger = "1234567890987654321212121232245354353465654645"
    val m = parse(" { \"number\": " + bigInteger + " } ").get
    m.getAnyInt("number") should equal (BigInt(bigInteger))
  }

  it should "read big decimals" in {
    val bigDecimal = "1234567890987654321212121232245354353465654645.5"
    val m = parse(" { \"number\": " + bigDecimal + " } ").get
    m.getAnyDecimal("number") shouldBe a [BigDecimal]
  }

}
