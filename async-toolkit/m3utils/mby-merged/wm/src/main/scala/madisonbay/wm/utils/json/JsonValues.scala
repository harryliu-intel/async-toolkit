package madisonbay.wm.utils.json

//scalastyle:off cyclomatic.complexity
object JsonValues {

  def toJsonValue(v: Any): Any = v match {
    case i: BigInt          => i.bigInteger
    case d: BigDecimal      => d.bigDecimal
    case s: Traversable[_]  => s.toList
    case _                  => v
  }

  def isValue(v: Any): Boolean = v match {
    case _: Boolean               => true
    case _: Byte                  => true
    case _: Char                  => true
    case _: Short                 => true
    case _: Int                   => true
    case _: Long                  => true
    case _: BigInt                => true
    case _: BigDecimal            => true
    case _: Float                 => true
    case _: Double                => true
    case _: String                => true
    case _: java.math.BigInteger  => true
    case _: java.math.BigDecimal  => true
    case _                        => false
  }

}
