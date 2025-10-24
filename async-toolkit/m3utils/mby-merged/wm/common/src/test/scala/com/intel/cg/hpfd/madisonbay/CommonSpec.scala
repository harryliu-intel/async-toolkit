// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package madisonbay

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
