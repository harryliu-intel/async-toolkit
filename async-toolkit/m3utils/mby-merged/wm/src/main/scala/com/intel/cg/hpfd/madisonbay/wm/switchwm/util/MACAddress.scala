package com.intel.cg.hpfd.madisonbay.wm.switchwm.util

object MACAddress {

  def apply(addr: Long): MACAddress = new MACAddress(addr)

  def apply(a0: Short, a1: Short, a2: Short): MACAddress = new MACAddress(a2.toLong << 32 & a1.toLong << 16 & a0.toLong)

}


/**
  * Ethernet Media Access Layer Address (48 bits)
  * @param addr
  */
class MACAddress(val addr: Long) extends AnyVal {
  // 'value' classes may not have constructors (not sure how to enforce invariant at runtime here)
  // assert((addr >> 48) == 0l, s"MAC Address cannot be more than 48 bits: $addr.toHexString")


  /**
    * @see https://securewiki.ith.intel.com/display/25T/PP+Mask+Generation
    * @return
    */
  def validSource: Boolean = !(
    (addr == 0) ||
      (addr == 0xffffff) ||
      ((addr & (0x1 << 40)) != 0) // multicast source if bit 40 set
    )

  override def toString: String = {
    def b(x: Int): String = {
      val byteVal = ((addr >> x * 8) & 0xff).toByte
      f"$byteVal%02X"
    }
    //scalastyle:off
    s"${b(5)}:${b(4)}:${b(3)}:${b(2)}:${b(1)}:${b(0)}"
    //scalastyle:on
  }

  /**
    * Mask off low order bits
    * @param bits number of bits to mask to zero from LSBs
    * @return new MACAddress with the mask applied
    */
  def maskLsb(bits: Int): MACAddress = {
    val mask = ~((1L << bits) - 1)
    MACAddress(mask & addr)
  }

}

