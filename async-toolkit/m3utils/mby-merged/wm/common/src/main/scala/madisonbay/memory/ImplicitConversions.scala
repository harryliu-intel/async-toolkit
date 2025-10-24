// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package madisonbay.memory


/** Some handy conversions */
object ImplicitConversions {
  /** Conversion to option for memory units. Works with indirect conversions too. */
  implicit def muToOption[A, M <: MemoryUnit](value: A)(implicit f: A => M) = Some(f(value))

  /** Conversion to option for alignment. Works with indirect conversions too. */
  implicit def alToOption[A](value: A)(implicit f: A => Alignment) = Some(f(value))

  /** Conversion to alignment for full bytes memory units. Works with indirect conversions too. */
  implicit def muToAl[A, M <: FullBytes](value: A)(implicit f: A => M): Alignment = f(value).toAlignment

  /** Conversion to addressing from option. Works with indirect conversions too. */
  implicit def optionToAddressing[A](value: A)(implicit f: A => Option[Addressing.Value]): Addressing.Value =
    f(value).getOrElse(Addressing.default)
}