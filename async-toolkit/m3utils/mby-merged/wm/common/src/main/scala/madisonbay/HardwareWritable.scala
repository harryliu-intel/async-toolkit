// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package madisonbay

/** Writable by hardware
  *
  * Applied to fields which can be generically written to by the hardware, allows writes of any value.
  */
trait HardwareWritable[P <: RdlField[P, E], E] { this: RdlField[P, E] =>
  def set(value: E): P = write(value)
  def modify(f: E => E): P = write(f(read()))
}
