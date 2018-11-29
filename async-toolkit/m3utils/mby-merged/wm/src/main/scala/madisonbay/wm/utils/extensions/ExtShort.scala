//scalastyle:off
package madisonbay.wm.utils.extensions

object ExtShort {

  implicit class Implicits(x: Short) extends ExtIntegers[Short] {

    override def addWithSaturation(number1: Short, number2: Long, limit: Long): Short =
      Math.min(number1 + number2, limit).toShort

    def reverse: Short = reverseShort(x)

    def addWithUShortSaturation(number2: Long): Short = super.addWithUShortSaturation(x, number2)

  }

  def reverseShort(s: Short): Short = {
    (((s >> 8) & 0xff) | (s << 8)).toShort
  }

}
