package com.intel.cg.hpfd.madisonbay.wm.switchwm.pipeline

import com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe.{PacketFields, PacketFlags}


object Metadata {

  def apply(flags : PacketFlags, fields : PacketFields): Metadata = new Metadata(flags, fields)

}

class Metadata(val flags : PacketFlags, val fields : PacketFields)
