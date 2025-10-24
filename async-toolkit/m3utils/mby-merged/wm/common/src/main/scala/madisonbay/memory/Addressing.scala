// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package madisonbay.memory


/** Addressing kind. Can be:
  *  * Compact --- aligned to accesswidth
  *  * Regalign --- aligned to regalign
  *  * Fullalign --- like regalign, but arrays aligned as a whole
  * */
object Addressing extends Enumeration {
  type Addressing = Value
  val Regalign, Compact, Fullalign = Value

  /** Default value (regalign) */
  val default = Regalign
}
