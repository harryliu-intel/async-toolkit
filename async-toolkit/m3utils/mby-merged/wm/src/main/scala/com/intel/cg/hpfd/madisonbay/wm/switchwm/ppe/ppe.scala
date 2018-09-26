
package com.intel.cg.hpfd.madisonbay.wm.switchwm.ppe

import scala.collection.immutable.BitSet

//scalastyle:off
object ppe {

//scalastyle:on
  class PortIndex(val p: Int) extends AnyVal

  class SourcePortMask(l: Long) {

    val bs = BitSet(l.toInt)

  }

  type EplRxFlags = Int

  class L3Domain(val d: Int) extends AnyVal

  class L2Domain(val d: Int) extends AnyVal

  //scalastyle:off
  object PacketClass extends Enumeration {
    val Unicast: Value = Value(0x1, "Unicast")
    val Broadcast: Value = Value(0x2, "Broadcast")
    val Multicast: Value = Value(0x4, "Multicast")
  }
  //scalastyle:on

  //scalastyle:off
  /**
    * ActionFlags are maintained through the PPE.
    *
    * Better removed from Scala and wiki and provided through code generation (i.e. RDL enum)?
    * @see https://securewiki.ith.intel.com/display/25T/PP+Mask+Generation
    */
  object ActionFlag extends Enumeration {
    /* Uncorrectable ECC or parity error detected while doing lookup */
    val DropPError: Value = Value(0, "DropPError")
    val Special: Value = Value(1,"Special")
    val DropParserErr: Value = Value(2, "DropParserErr")
    // ...
    /* Frame was mirrored as a result of a CGRP action. */
    val MirrorIngressCGRP: Value = Value(43, "MirrorIngressCGRP")
    /* Reserved */
    val Reserved: Value = Value(44, "Reserved")
  }
  //scalastyle:on

  // VLAN Id
  class VID(val vid: Int) extends AnyVal

  //scalastyle:off
  /**
    * 8 Bit
    * @param cgrp
    */
  class CGRP(val cgrp: Int) extends AnyVal {

    def == (other: Long, otherMask: Long): Boolean = {
      (cgrp & otherMask) == (other & otherMask)
    }

  }
  //scalastyle:on

  /**
    * 16-bit glort (Global Resource Tag) identifies a logical port or multicast group of link aggregation group
    *   or load-balancing group.
    * @param glort
    */
  class GLORT(val glort: Short) extends AnyVal {

    def maskUpdate(newGlort: Short, newGlortMask: Short): GLORT = {
      // take zero out the masked bits, and bitwise-OR in those that are being updated
      val r = (glort & ~newGlortMask) | (newGlort & newGlortMask)
      new GLORT(r.toShort)
    }

  }

  class TrafficClass(val tc: Int) extends AnyVal

}
