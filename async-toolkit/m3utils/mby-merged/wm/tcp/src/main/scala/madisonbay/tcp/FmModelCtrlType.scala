// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package madisonbay.tcp

object FmModelCtrlType extends Enumeration {
  val Length = 1

  val ChipResetReq = Value(1, "ChipResetReq")
  val ChipResetRep = Value(2, "ChipResetRep")

  implicit val encoder: ByteArrayEncoder[FmModelCtrlType.Value] =
    value => ByteArrayEncoder.u8bae.encode(value.id.byteValue())
}

