package madisonbay.wm.utils.defs

object IndexedConstant {

  trait Element {
    def index: Option[Int]
  }

  trait Container {

    val constants: List[Element]

    def getConstant(index: Int): Option[Element] = constants.find(_.index.contains(index))

    def getConstantIndex(constant: Element): Option[Int] = constant.index

  }

}
