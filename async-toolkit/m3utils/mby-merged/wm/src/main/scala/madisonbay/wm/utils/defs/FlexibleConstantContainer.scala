package madisonbay.wm.utils.defs

abstract class FlexibleConstantContainer[KeyType: Ordering] {
  trait Element {
    val index: KeyType
  }

  type ElementType <: Element

  val definedConstants: List[ElementType]

  def wildcard(key: KeyType): ElementType

  def getConstant(key: KeyType): ElementType = definedConstants.find(_.index == key).getOrElse(wildcard(key))

  implicit val ordering: Ordering[ElementType] = new Ordering[ElementType] {
    def compare(a: ElementType, b: ElementType): Int = Ordering[KeyType].compare(a.index, b.index)
  }
}
