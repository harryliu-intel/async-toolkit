// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package madisonbay.tcp

import com.intel.cg.hpfd.csr.macros.SizedArray
import eu.timepit.refined.W
import madisonbay.PrimitiveTypes._

case class FmModelMsgAttr(
  Type: FmModelAttrType.Value,
  Keylength: U16,
  Key: SizedArray[U8,W.`256`.T],
  Value: SizedArray[U8, W.`256`.T]
)

