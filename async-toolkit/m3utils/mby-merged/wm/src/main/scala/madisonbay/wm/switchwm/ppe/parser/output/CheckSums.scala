// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package madisonbay.wm.switchwm.ppe.parser.output

import madisonbay.wm.switchwm.epl.PacketHeader.IPv4CorrectCheckSum

case class CheckSums(checkSumOk: Option[IPv4CorrectCheckSum], drop: Boolean)
