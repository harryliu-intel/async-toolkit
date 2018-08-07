package switch_wm

package object ppe {
  class PortIndex(val p : Int) extends AnyVal

  class L3Domain(val d : Int) extends AnyVal
  class L2Domain(val d : Int) extends AnyVal

  object PacketClass extends Enumeration {
    val Unicast = Value(0x1, "Unicast")
    val Broadcast = Value(0x2, "Broadcast")
    val Multicast = Value(0x4, "Multicast")
  }

  /**
    * ActionFlags are maintained through the PPE.
    *
    * Better removed from Scala and wiki and provided through code generation (i.e. RDL enum)?
    * @see https://securewiki.ith.intel.com/display/25T/PP+Mask+Generation
    */
  object ActionFlag extends Enumeration {
    /* Uncorrectable ECC or parity error detected while doing lookup */
    val DropPError = Value(0, "DropPError")
    val Special = Value(1,"Special")
    val DropParserErr = Value(2, "DropParserErr")
    // ...
    /* Frame was mirrored as a result of a CGRP action. */
    val MirrorIngressCGRP = Value(43, "MirrorIngressCGRP")
    /* Reserved */
    val Reserved = Value(44, "Reserved")
  }

  class VID(val vid : Int) extends AnyVal

  /**
    * Ethernet Media Access Layer Address (48 bits)
    * @param addr
    */
  class MACAddress(val addr : Long) extends AnyVal {
    // 'value' classes may not have constructors (not sure how to enforce invariant at runtime here)
    // assert((addr >> 48) == 0l, s"MAC Address cannot be more than 48 bits: $addr.toHexString")


    /**
      * @see https://securewiki.ith.intel.com/display/25T/PP+Mask+Generation
      * @return
      */
    def validSource : Boolean = !(
      (addr == 0) ||
      (addr == 0xffffff) ||
        ((addr & (0x1 << 40)) != 0) // multicast source if bit 40 set
      )

    override def toString(): String = {
      def b(x : Int) : String = ((addr >> x * 8) & 0xff).toByte.toHexString
      s"${b(0)}:${b(1)}:${b(2)}:${b(3)}:${b(4)}:${b(5)}"
    }
  }

  /**
    * 8 Bit
    * @param cgrp
    */
  class CGRP(val cgrp : Int) extends AnyVal

  class GLORT(val glort : Int) extends AnyVal
}
