//scalastyle:off
package com.intel.cg.hpfd.csr

import com.intel.cg.hpfd.csr.testData.{
  AtAddressingExample,
  ModuloAddressingExample,
  IncrementAddressingExample,
  AllAddressingExample,
  NonContiguousAddressingExample
}
import com.intel.cg.hpfd.madisonbay.Memory._
import org.scalatest.{FlatSpec, Matchers}
import scala.language.postfixOps

class InitializationMacroTest extends FlatSpec with Matchers
    with AtAddressingExample
    with ModuloAddressingExample
    with IncrementAddressingExample
    with AllAddressingExample
    with NonContiguousAddressingExample {

  def testCaseScenario[A <: Product with Serializable](result: Address => A, expectedValue: A) {
    val initialAddress = Address(0,0 bits)
    result(initialAddress) shouldEqual expectedValue
  }

  "@Initialize macro" should "properly assign addresses among csr hierarchy" in {
    testCaseScenario(testData.all.AddressMap.apply, allAddressingExpectedValue)
  }

  it should "properly handle @ modifier for some dummy hierarchy" in {
    testCaseScenario(testData.at.AddressMap.apply, atAddressingExpectedValue)
  }

  it should "properly handle %= modifier for some dummy hierarchy" in {
    testCaseScenario(testData.modulo.AddressMap.apply, moduloAddressingExpectedValue)
  }

  it should "properly handle += modifier for some dummy hierarchy" in {
    testCaseScenario(testData.increment.AddressMap.apply, incrementAddressingExpectedValue)
  }

  it should "properly handle addressing for non contiguous word alignment" in {
    testCaseScenario(testData.nonContiguous.AddressMap.apply, nonContiguousAddressingExpectedValue)
  }
}
