// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package madisonbay.tcp

import madisonbay.PrimitiveTypes._

case class FmModelMsgMgmt32(
  Type: FmModelMgmtType.Value,
  Address: U32,
  Value: U64
)

