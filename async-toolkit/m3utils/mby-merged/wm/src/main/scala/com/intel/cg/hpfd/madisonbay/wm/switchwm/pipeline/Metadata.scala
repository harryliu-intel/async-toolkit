package com.intel.cg.hpfd.madisonbay.wm.switchwm.pipeline

import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.{PacketFields, PacketFlags}


case class Metadata(flags: PacketFlags, fields: PacketFields)
