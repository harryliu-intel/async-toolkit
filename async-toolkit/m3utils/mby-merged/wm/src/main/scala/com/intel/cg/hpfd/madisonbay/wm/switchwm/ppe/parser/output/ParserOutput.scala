package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.output

import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.mapper.PacketFields
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.parser.Parser.ProtoOffsets
import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.ppe.{EplRxFlags, PortIndex}

case class ParserOutput(rxPort: PortIndex,
                       pktMeta: Int,
                       rxFlags: EplRxFlags,
                       segMetaErr: Boolean,
                       paAdjSegLegLen: Int,
                       paKeys: PacketFields, // should be 'option' values?
                       paKeysValid: Boolean,
                       paFlags: PacketFlags,
                       paPointers: ProtoOffsets,
                       paPointersValid: Boolean,
                       paCsumOk: Boolean,
                       paExceptionStage: Int,
                       paExceptionDepthExceeded: Boolean,
                       paExceptionTruncHeader: Boolean,
                       paExParsingDone: Boolean,
                       paDrop: Boolean,
                       paPacketType: Int
                      )
