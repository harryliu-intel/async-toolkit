// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package madisonbay.tcp

import com.intel.cg.hpfd.csr.macros.SizedArray
import madisonbay.PrimitiveTypes._
import eu.timepit.refined.W

case class FmModelMsgGetInfo(
  Type: FmModelInfoType.Value,
  Nportssupported: U16,
  Padding: SizedArray[U8, W.`51`.T]
)

