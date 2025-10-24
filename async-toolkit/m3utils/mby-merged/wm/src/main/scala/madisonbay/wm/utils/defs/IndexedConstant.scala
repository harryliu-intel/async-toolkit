// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package madisonbay.wm.utils.defs

object IndexedConstant {

  trait Element {
    def index: Option[Int]
  }

  trait Container {

    val constants: List[Element]

    def getConstant(index: Int): Option[Element] = constants.find(_.index.contains(index))

    def getConstantIndex(constant: Element): Option[Int] = constant.index

  }

}
