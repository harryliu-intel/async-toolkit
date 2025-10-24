// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package madisonbay.wm.utils.extensions

object ExtLong {

  implicit class Implicits(x: Long) extends ExtIntegers[Long] {

    override def addWithSaturation(number1: Long, number2: Long, limit: Long): Long =
      Math.min(number1 + number2, limit)

    def incWithUByteSaturation: Long = super.incWithUByteSaturation(x)

    def getBit(pos: Int): Boolean = (0x1 & x >> pos) == 1

  }

}
