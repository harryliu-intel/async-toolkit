package com.intel.cg.hpfd.madisonbay

import org.scalatest.{WordSpec, Inspectors, Matchers}
import org.scalatest.Inspectors.{forAll => _}
import org.scalatest.prop._
import org.scalatest.prop.Configuration.{PropertyCheckConfigParam, PropertyCheckConfig}

trait CommonSpec extends WordSpec with Matchers with Inspectors with GeneratorDrivenPropertyChecks {
  val given = afterWord("given")
  val is = afterWord("is")
  val handle = afterWord("handle")
  val containing = afterWord("containing")
}
