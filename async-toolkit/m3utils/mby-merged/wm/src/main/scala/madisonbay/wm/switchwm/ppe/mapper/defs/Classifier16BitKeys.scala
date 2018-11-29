package madisonbay.wm.switchwm.ppe.mapper.defs

import madisonbay.wm.utils.defs.FlexibleConstantContainer

object Classifier16BitKeys extends FlexibleConstantContainer[Int] {
  sealed trait Classifier16BitKey extends Element

  case object InnerSourceMac0            extends Classifier16BitKey {val index = 3}
  case object InnerSourceMac1            extends Classifier16BitKey {val index = 4}
  case object InnerSourceMac2            extends Classifier16BitKey {val index = 5}

  case object OuterDestinationMac0       extends Classifier16BitKey {val index = 6}
  case object OuterDestinationMac1       extends Classifier16BitKey {val index = 7}
  case object OuterDestinationMac2       extends Classifier16BitKey {val index = 8}

  case object OuterSourceMac0            extends Classifier16BitKey {val index = 9}
  case object OuterSourceMac1            extends Classifier16BitKey {val index = 10}
  case object OuterSourceMac2            extends Classifier16BitKey {val index = 11}

  case object OuterEthertype             extends Classifier16BitKey {val index = 12}
  case object MappedNibbles0             extends Classifier16BitKey {val index = 13}
  case object OuterVlan1                 extends Classifier16BitKey {val index = 14}
  case object OuterVlan2                 extends Classifier16BitKey {val index = 15}
  case object OuterL4Source              extends Classifier16BitKey {val index = 16}
  case object OuterL4Destination         extends Classifier16BitKey {val index = 17}

  override type ElementType = Classifier16BitKey

  override val definedConstants: List[Classifier16BitKey] = List(
    InnerSourceMac0,
    InnerSourceMac1,
    InnerSourceMac2,
    OuterDestinationMac0,
    OuterDestinationMac1,
    OuterDestinationMac2,
    OuterSourceMac0,
    OuterSourceMac1,
    OuterSourceMac2,
    MappedNibbles0,
    OuterEthertype,
    OuterVlan1,
    OuterVlan2,
    OuterL4Source,
    OuterL4Destination
  )

  case class Unknown16BitKey(index: Int) extends Classifier16BitKey

  override def wildcard(key: Int): Classifier16BitKey = Unknown16BitKey(key)
}
