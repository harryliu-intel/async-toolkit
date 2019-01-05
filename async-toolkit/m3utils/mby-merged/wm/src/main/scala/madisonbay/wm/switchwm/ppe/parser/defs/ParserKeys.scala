package madisonbay.wm.switchwm.ppe.parser.defs

import madisonbay.wm.utils.defs.FlexibleConstantContainer

object ParserKeys extends FlexibleConstantContainer[Int] {
  sealed trait ParserKey extends Element
  case object InnerSourceMac0            extends ParserKey {val index = 3}
  case object InnerSourceMac1            extends ParserKey {val index = 4}
  case object InnerSourceMac2            extends ParserKey {val index = 5}
  case object OuterDestinationMac0       extends ParserKey {val index = 6}
  case object OuterDestinationMac1       extends ParserKey {val index = 7}
  case object OuterDestinationMac2       extends ParserKey {val index = 8}
  case object OuterSourceMac0            extends ParserKey {val index = 9}
  case object OuterSourceMac1            extends ParserKey {val index = 10}
  case object OuterSourceMac2            extends ParserKey {val index = 11}
  case object OuterEthertype             extends ParserKey {val index = 12}
  case object OuterVlan1                 extends ParserKey {val index = 14}
  case object OuterVlan2                 extends ParserKey {val index = 15}
  case object OuterL4Source              extends ParserKey {val index = 16}
  case object OuterL4Destination         extends ParserKey {val index = 17}
  case object OuterIPv4Header            extends ParserKey {val index = 42}
  case object OuterIPv6Header            extends ParserKey {val index = 44}
  case object OuterIpSourceAddress0      extends ParserKey {val index = 48}
  case object OuterIpSourceAddress1      extends ParserKey {val index = 49}
  case object OuterIpDestinationAddress0 extends ParserKey {val index = 50}
  case object OuterIpDestinationAddress1 extends ParserKey {val index = 51}
  case object L4CheckSum                 extends ParserKey {val index = 32}
  case object L4Length                   extends ParserKey {val index = 35}

  type ElementType = ParserKey

  override val definedConstants: List[ParserKey] = List(
    InnerSourceMac0,
    InnerSourceMac1,
    InnerSourceMac2,
    OuterDestinationMac0,
    OuterDestinationMac1,
    OuterDestinationMac2,
    OuterSourceMac0,
    OuterSourceMac1,
    OuterSourceMac2,
    OuterEthertype,
    OuterVlan1,
    OuterVlan2,
    OuterL4Source,
    OuterL4Destination,
    OuterIPv4Header,
    OuterIPv6Header,
    OuterIpSourceAddress0,
    OuterIpSourceAddress1,
    OuterIpDestinationAddress0,
    OuterIpDestinationAddress1,
    L4CheckSum,
    L4Length)

  case class UnknownParserKey(index: Int) extends ParserKey

  override def wildcard(key: Int): ParserKey = UnknownParserKey(key)
}
