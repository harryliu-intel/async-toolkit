// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package madisonbay.tcp

import madisonbay.PrimitiveTypes._
import com.intel.cg.hpfd.csr.macros.SizedArray
import eu.timepit.refined.W

case class FmModelSideBandData(Idtag: U32, Tc: U8, Pktmeta: SizedArray[U8, W.`32`.T])

