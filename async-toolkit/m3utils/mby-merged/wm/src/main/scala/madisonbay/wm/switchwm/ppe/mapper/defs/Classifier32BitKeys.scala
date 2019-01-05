package madisonbay.wm.switchwm.ppe.mapper.defs

import madisonbay.wm.utils.defs.FlexibleConstantContainer

object Classifier32BitKeys extends FlexibleConstantContainer[Int] {
  sealed trait Classifier32BitKey extends Element
  case object OuterSourceIp       extends Classifier32BitKey { val index = 0 }
  case object OuterDestiantionIp  extends Classifier32BitKey { val index = 1 }

  override type ElementType = Classifier32BitKey

  override val definedConstants: List[Classifier32BitKey] = List(OuterSourceIp, OuterDestiantionIp)

  case class Unknown32BitKey(index: Int) extends Classifier32BitKey

  override def wildcard(key: Int): Classifier32BitKey = Unknown32BitKey(key)
}
