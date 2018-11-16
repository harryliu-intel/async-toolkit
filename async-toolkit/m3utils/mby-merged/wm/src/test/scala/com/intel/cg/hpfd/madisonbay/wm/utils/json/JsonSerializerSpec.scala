package com.intel.cg.hpfd.madisonbay.wm.utils.json

import org.scalatest.{FlatSpec, Matchers}
import com.intel.cg.hpfd.madisonbay.wm.utils.json.JsonSerializer._

//scalastyle:off magic.number
class JsonSerializerSpec extends FlatSpec with Matchers {

  case class TestClass1(value: Int)
  class TestClass2(values: List[Int], maps: Map[Int, String], myDouble: Double) {
    def get: (List[Int], Map[Int, String], Double) = (values, maps, myDouble)
  }
  case class TestClass3(x: String, tc1: TestClass1)
  case class TestClass4(testClass3: TestClass3, testClass2: TestClass2)

  sealed trait Flags
  case object Flag_abc extends Flags
  case object Flag_xyz extends Flags
  case class ClassFlags(flags: Map[String, Flags])
  case class ClassFlagsList(flagsList: List[Flags])
  class AllFlags(flags: ClassFlags, flagsList: ClassFlagsList) { def get: (ClassFlags, ClassFlagsList) = (flags, flagsList) }

  it should "throw exception on AnyVal, String and collections" in {
    assertThrows[IllegalArgumentException] {
      toMap(5)
    }

    assertThrows[IllegalArgumentException] {
      toMap("test")
    }

    assertThrows[IllegalArgumentException] {
      toMap(List(1,2,3))
    }
  }

  it should "convert simple case class" in {
    toMap(TestClass1(5)) shouldEqual Map("value"->5)
  }

  it should "convert collections" in {
    val m1 = Map(1->"one", 2->"two")
    val d1 = 0.55

    toMap(new TestClass2(List(1,2,3), m1, d1)) shouldEqual Map("values"->List(1,2,3), "maps"->Map(1->"one", 2->"two"), "myDouble"->0.55)
  }

  it should "convert nested classes" in {
    toMap(TestClass4(TestClass3("test3", TestClass1(3)), new TestClass2(List(), Map(), 0.0))) shouldEqual Map(
      "testClass3"->Map("x"->"test3", "tc1"->Map("value"->3)),
      "testClass2"->Map("values"->List(), "maps"->Map(), "myDouble"->0.0)
      )
  }

  it should "convert nested classes with case classes" in {
    toMapWithCaseClasses(TestClass4(TestClass3("test3", TestClass1(3)), new TestClass2(List(), Map(), 0.0))) shouldEqual Map(
      "TestClass3"->Map("x"->"test3", "TestClass1"->Map("value"->3)),
      "testClass2"->Map("values"->List(), "maps"->Map(), "myDouble"->0.0)
    )
  }

  it should "convert collection of case objects" in {

    val flags = Map("flag18"->Flag_abc, "flag24"->Flag_xyz)
    toMapWithCaseClasses(new AllFlags(ClassFlags(flags), ClassFlagsList(List(Flag_xyz, Flag_abc)))) shouldEqual Map(
      "ClassFlags"->Map(
        "flags"->Map(
          "flag18"->"Flag_abc",
          "flag24"->"Flag_xyz")
      ),
      "ClassFlagsList"->Map(
        "flagsList"->List("Flag_xyz", "Flag_abc")
      )
    )
  }

}
