//scalastyle:off
package com.intel.cg.hpfd.csr

import com.intel.cg.hpfd.csr.testData.AtAddressingExample
import com.intel.cg.hpfd.madisonbay.Memory._
import org.scalatest.{FlatSpec, Inside, Matchers}
import com.intel.cg.hpfd.csr.testData.optics._
import com.intel.cg.hpfd.madisonbay.BitVector
import org.scalatest.OptionValues._
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalatest._
import prop._
import monocle.Optional

class GenOpticsLookupMacroTest extends PropSpec with PropertyChecks with Matchers with Inside with AtAddressingExample {

  val initialAddress = Address(0, 0 bits)
  val optionalId = Optional.id[AddressMap]
  val lastRegisterAddress = Address at 0x78.bytes

  property("Setting any long value at specified Optional optics path should work as expected") {
    forAll { (expectedValue: Long) =>
      val topAddressMap = AddressMap(initialAddress)

      val opticsPath = AddressMap
        .genOpticsLookup(topAddressMap, optionalId)
        .get(lastRegisterAddress)
        .value

      val modifiedTopAddressMap = opticsPath.set(BitVector(expectedValue))(topAddressMap)

      inside(modifiedTopAddressMap) { case AddressMap(_,_,regFileB) =>
        inside(regFileB) { case RegisterFileB(_,_,register) =>
          register.serialize.toLong shouldEqual expectedValue
          register.serialize shouldEqual opticsPath.getOption(modifiedTopAddressMap).value
        }
      }
    }
  }
}
