// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package madisonbay

/** Readable by hardware
  *
  * Applied to RdlFields which can be read by hardware (no known examples of cases where this trait does not apply)
  */
trait HardwareReadable[E] { this: RdlField[_, E] =>
  def apply(): E = read()
  def get(): E = read()
}
