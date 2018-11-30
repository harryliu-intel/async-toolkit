package madisonbay.memory


/** Contiguous range of addresses.
  *
  * @param pos starting poing of the range
  * @param width width in bits
  * */
case class AddressRange private (pos: Address, width: Bits) {
  require(width.toLong > 0L, s"address range must have positive length, found: $width ($pos until $lim)")  // pos < lim

  /** First address free after this range,
    * e.g. AddressRange(Address(32.bytes), 8.bytes).lim == Address(40.bytes)
    */
  def lim: Address = pos + width

  /** Position and the next position after the range,
    * e.g. AddressRange(Address(32.bytes), 8.bytes).toAddressPair == (Address(32.bytes), Address(40.bytes)) */
  def toAddressPair: (Address, Address) = (pos, lim)

  override def toString: String = "AddressRange(pos: " + pos + ", lim: " + lim + ")"

  /** Highly friendly and readable range form */
  def rangeString: String = "" + pos.toBits.toLong + ".." + lim.toBits.toLong
}
object AddressRange {
  /** Simple explicit constructor */
  def apply[M <: MemoryUnit](addr: Address, munit: M): AddressRange = new AddressRange(addr, munit.toBits)

  /** Construct from two addresses. */
  def apply(pos: Address, lim: Address): AddressRange = AddressRange(pos, (lim-pos).toBits)

  /** Field constructor */
  def makeField[M <: MemoryUnit](addr: Address, munit: M): AddressRange = AddressRange(addr, munit.toBits)

  /** Register constructor.
    *
    * Arguments' meaning and relation to addressing --- @see Addressing
    */
  def placeReg(at: Address,
               regwidth: Alignment,
               alignment: Option[Alignment] = None,
               accesswidth: Option[Alignment] = None,
               addressing: Addressing.Value = Addressing.default): AddressRange = {
    // 1. addressing is compact => use accesswidth (otherwise use regwidth)
    // 2. default accesswidth == regwidth
    // 3. if explicit alignment is specified, use it instead
    // 4. accesswidth must be no bigger than regwidth
    accesswidth.foreach(x => require(x <= regwidth))
    val width = accesswidth
      .filter(_ => addressing == Addressing.Compact)
      .getOrElse(regwidth)
    val align = alignment.getOrElse(width)
    val lo = at alignTo align
    AddressRange(lo, regwidth.toBits)
  }
}
