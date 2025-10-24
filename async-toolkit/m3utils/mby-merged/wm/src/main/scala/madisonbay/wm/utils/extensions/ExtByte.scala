// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package madisonbay.wm.utils.extensions

object ExtByte {

  implicit class Implicits(byte: Byte) {

    def hex: String = {
      f"0x$byte%02X"
    }

  }

}
