package switch_wm

import switch_wm.ppe.Parser.ProtoOffsets

import scala.collection.immutable.BitSet

package object ppe {
  class PortIndex(val p : Int) extends AnyVal


  class SourcePortMask(l : Long) {
    val bs = BitSet(l.toInt)

  }

  type EplRxFlags = Int

  class L3Domain(val d : Int) extends AnyVal
  class L2Domain(val d : Int) extends AnyVal

  object IPVersion extends Enumeration  {
    val V4 = Value(0x4, "IPv4")
    val V6 = Value(0x6, "IPv4")
  }

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
      def b(x : Int) : String = {
        val byteVal = ((addr >> x * 8) & 0xff).toByte
        f"$byteVal%02X"
      }
      s"${b(5)}:${b(4)}:${b(3)}:${b(2)}:${b(1)}:${b(0)}"
    }

    /**
      * Mask off low order bits
      * @param bits number of bits to mask to zero from LSBs
      * @return new MACAddress with the mask applied
      */
    def maskLsb(bits : Int) : MACAddress = {
      val mask = ~((1l << bits) - 1)
      MACAddress(mask & addr)
    }
  }
  object MACAddress {
    def apply(addr : Long) = new MACAddress(addr)
    def apply(a0: Short, a1: Short, a2: Short) = new MACAddress(a2.toLong << 32 & a1.toLong << 16 & a0.toLong)

  }

  /**
    * 8 Bit
    * @param cgrp
    */
  class CGRP(val cgrp : Int) extends AnyVal {
    def == (other : Long, otherMask : Long) : Boolean = {
      (cgrp & otherMask) == (other & otherMask)
    }
  }

  /**
    * 16-bit glort
    * @param glort
    */
  class GLORT(val glort : Short) extends AnyVal {
    def maskUpdate(newGlort : Short, newGlortMask : Short) : GLORT = {
      // take zero out the masked bits, and bitwise-OR in those that are being updated
      val r = (glort & ~newGlortMask) | (newGlort & newGlortMask)
      new GLORT(r.toShort)
    }
  }

  class TrafficClass(val tc : Int) extends AnyVal



  case class ParserOutput (rxPort : PortIndex,
                           pktMeta : Int,
                           rxFlags : EplRxFlags,
                           segMetaErr : Boolean,
                           paAdjSegLegLen : Int,
                           paKeys : PacketFields, // should be 'option' values?
                           paKeysValid : Boolean,
                           paFlags : PacketFlags,
                           paPointers: ProtoOffsets,
                           paPointersValid : Boolean,
                           paCsumOk : Boolean,
                           paExceptionStage : Int,
                           paExceptionDepthExceeded : Boolean,
                           paExceptionTruncHeader : Boolean,
                           paExParsingDone : Boolean,
                           paDrop : Boolean,
                           paPacketType : Int
                          )
  implicit def nibblesKey16(n : (Int, Int, Int, Int)) : Key16 = {
    (n._1 | n._2 << 4 | n._3 << 8 | n._4 << 12).toShort
  }

  //Table 4 - Fields and classification granularity
  //  0..63	16b	KEY16s (0..31)
  //64..95	8b	KEY8s (0..31)
  //96..159	32b	KEY32s (0..15)
  type Key16 = Short
  type Key8 = Byte
  type Key32 = Int

  class FieldVector {
    val array = Array.ofDim[Byte](160)
    def update(i: Int)(k : Key8) { array(i) = k }
    def k16(i : Int): Key16 = {
      require(i %2 == 0)
      require(i < 64)

      array(i).toShort
    }
    def k32(i : Int): Key32 = {
      require(i %4 == 0)
      array(i).toInt
    }
    def k8(i : Int): Key32 = {
      require(i %4 == 0)
      array(i).toInt
    }
  }

}
