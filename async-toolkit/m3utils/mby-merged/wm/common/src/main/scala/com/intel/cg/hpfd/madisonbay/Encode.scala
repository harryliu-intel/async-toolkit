//scalastyle:off
package com.intel.cg.hpfd.madisonbay

import scala.reflect.ClassTag


/** Encoding type class for [[RdlField]].
  *
  * Provides default value, conversions in both sides and minimal and maximal size of the field.
  */
abstract class Encode[T: ClassTag] {
  /** Default (reset) value.
    *
    * Could be computable.
    * Overridable for fields with the said encoding.
    */
  def default: T

  /** Conversion to raw machine word. */
  def toRaw(t: T): Long

  /** Conversion from raw machine word. */
  def fromRaw(l: Long): T

  /** Minimal size of the field. */
  val minSize: Int

  /** Maximal size of the field.
    *
    * Should be no smaller, and in many cases equal, to minSize.
    */
  val maxSize: Int

  /** Underlying type's stringification. */
  final def typeStr: String = s"${implicitly[ClassTag[T]]}"

  override def toString: String = s"Encoding[$typeStr]"
}

object Encode {
  /** Shortcut for {{{ implicitly[Encode[E]] }}}. */
  def encode[E: Encode]: Encode[E] = implicitly[Encode[E]]

  /** General encoding constructor */
  def apply[T: ClassTag](mi: Int, ma: Int, d: => T)(to: T => Long)(from: Long => T): Encode[T] = new Encode[T] {
    def default: T = d
    def toRaw(t: T): Long = to(t)
    def fromRaw(l: Long): T = from(l)
    val minSize = mi
    val maxSize = ma
  }

  /** Simplified encoding constructor */
  def apply[T: ClassTag](size: Int, d: => T)(to: T => Long)(from: Long => T): Encode[T] = new Encode[T] {
    def default: T = d
    def toRaw(t: T): Long = to(t)
    def fromRaw(l: Long): T = from(l)
    val minSize = size
    val maxSize = size
  }

  // enums
  implicit val encodeBoolean = Encode(1, 1, false)(if (_) 1L else 0L)(_ != 0)

  // integers
  implicit val encodeByte = Encode(1, 8, 0.toByte)(_.toLong)(_.toByte)
  implicit val encodeShort = Encode(1, 16, 0.toShort)(_.toLong)(_.toShort)
  implicit val encodeInt = Encode(1, 32, 0)(_.toLong)(_.toShort)
  implicit val encodeLong = Encode(1, 64, 0x00l)(x => x)(x => x)

  // floating points
  implicit val encodeFloat = Encode(32, 0.0f)(_.toLong)(_.toFloat)
  implicit val encodeDouble = Encode(64, 0.0d)(_.toLong)(_.toDouble)
}
