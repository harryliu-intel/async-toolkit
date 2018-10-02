//scalastyle:off
package com.intel.cg.hpfd.madisonbay.wm.extensions

object ExtShort {

  def reverseShort(s: Short): Short = {
    (((s >> 8) & 0xff) | (s << 8)).toShort
  }

}
