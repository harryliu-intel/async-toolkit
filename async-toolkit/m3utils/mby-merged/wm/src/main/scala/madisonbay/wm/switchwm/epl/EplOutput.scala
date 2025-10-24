// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package madisonbay.wm.switchwm.epl

import madisonbay.wm.switchwm.ppe.ppe.Port

case class EplOutput(rxData: Packet,
                     rxPort: Port,
                     pktMeta: Int)
