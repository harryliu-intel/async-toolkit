package madisonbay.wm.utils.defs

trait FlexibleConstantContainer[KeyType] {
  trait Element {
    val index: KeyType
  }

  type ElementType <: Element

  val definedConstants: List[ElementType]

  def wildcard(key: KeyType): ElementType

  def getConstant(key: KeyType): ElementType = definedConstants.find(_.index == key).getOrElse(wildcard(key))
}
