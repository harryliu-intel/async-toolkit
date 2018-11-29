//scalastyle:off
package com.intel.cg.hpfd.madisonbay

import com.intel.cg.hpfd.madisonbay.Encode.encode

import scala.reflect.ClassTag


/** Encoding type class for [[RdlField]].
  *
  * Provides default value, conversions in both sides and minimal and maximal size of the field.
  */
abstract class Encode[T: ClassTag] {
  /** Default (reset) value.
    *
    * Could be computable or overrode by "higher" (from Lens' perspective) type.
    */
  def default: T

  /** Conversion to raw bits. */
  def toRaw(value: T): BitVector

  /** Conversion from raw bits. */
  def fromRaw(bits: BitVector): T

  /** Size in bits.
    *
    * Could be overrode by a "higher" (from Lens' perspective) type,
    * but care should be taken. In practice, only Raw (corresponding to Long)
    * encoding should be overrode.
    */
  def size: Int

  /** Underlying type's stringification. */
  final def typeStr: String = s"${implicitly[ClassTag[T]]}"

  override def toString: String = s"Encoding[$typeStr]"
}

object Encode {
  /** Shortcut for {{{ implicitly[Encode[E]] }}}. */
  def encode[E: Encode]: Encode[E] = implicitly[Encode[E]]

  /** Simplified encoding constructor using single raw word. */
  def simple[T: ClassTag](siz: Int, d: => T)(to: T => Long)(from: Long => T): Encode[T] = {
    require( siz >= 0 )
    require( siz <= 64 )
    new Encode[T] {
      def size: Int = siz
      def default: T = d
      def toRaw(value: T): BitVector = BitVector(to(value), size)
      def fromRaw(bits: BitVector): T = {
        require(bits.length == siz)
        from(bits.toRaw(0))
      }
    }
  }

  /** Simplified encoding constructor using single raw word, default value from 0L. */
  def simple[T: ClassTag](siz: Int)(to: T => Long)(from: Long => T): Encode[T] = {
    simple(siz, from(0L))(to)(from)
  }

  /** Encoding constructor using any number of raw bits. */
  def apply[T: ClassTag](siz: Int, d: => T)(to: T => BitVector)(from: BitVector => T): Encode[T] = {
    require( siz >= 0 )
    new Encode[T] {
      def size: Int = siz
      def default: T = d
      def toRaw(value: T): BitVector = to(value)
      def fromRaw(bits: BitVector): T = {
        require(bits.length == siz)
        from(bits)
      }
    }
  }

  /** Encoding constructor using any number of raw bits, default value from zero BitVector. */
  def apply[T: ClassTag](siz: Int)(to: T => BitVector)(from: BitVector => T): Encode[T] = {
    apply(siz, from(BitVector.zeros(siz)))(to)(from)
  }

  // enums
  implicit val encodeBoolean: Encode[Boolean] = Encode.simple[Boolean](1)(if (_) 1L else 0L)(_ != 0)

  // integers
  implicit val encodeByte: Encode[Byte] = Encode.simple[Byte](8)(_.toLong)(_.toByte)
  implicit val encodeShort: Encode[Short] = Encode.simple[Short](16)(_.toLong)(_.toShort)
  implicit val encodeInt: Encode[Int] = Encode.simple[Int](32)(_.toLong)(_.toInt)
  implicit val encodeLong: Encode[Long] = Encode.simple[Long](64)(identity)(identity)

  /** raw bit value with no meaning. */
  def encodeRaw(size: Int): Encode[Long] = Encode.simple[Long](size)(identity)(identity)

  /** Encodable extensions */
  implicit class EncodeToBvec[E: Encode](arg: E) {
    /** Convert to a BitVector */
    def toBitVector(): BitVector = encode[E].toRaw(arg)
  }

  /** Traversable of encodables extensions */
  implicit class TravEncodeToBVec[E: Encode](arg: Traversable[E]) {
    /** Convert to a BitVector */
    def toBitVector(): BitVector = arg.map(x => encode[E].toRaw(x)).toBitVector
  }

  /** Iterator of encodables extensions */
  implicit class ItEncodeToBVec[E: Encode](arg: Iterator[E]) {
    /** Convert to a BitVector */
    def toBitVector(): BitVector = arg.map(x => encode[E].toRaw(x)).toBitVector
  }
}

/** Encodable value with its encoding. Wraps encoding operations. */
case class Encodable[E](value: E)(implicit val encode: Encode[E]) {
  def toRaw(): BitVector = encode.toRaw(value)
  def fromRaw(bits: BitVector): Encodable[E] = copy(value = encode.fromRaw(bits))
  def default(): Encodable[E] = copy(value = encode.default)
  def size: Int = encode.size
}
object Encodable {
  /** Automatic conversion. */
  implicit def fromValue[E: Encode](value: E): Encodable[E] =
    new Encodable[E](value)(Encode.encode[E])

  implicit def c2c[E: Encode](container: Traversable[E]): Traversable[Encodable[E]] =
    container.map(x => fromValue(x))

  /** Manual conversion. */
  implicit class ToEncodable[E](value: E) {
    def toEncodable(implicit encode: Encode[E]): Encodable[E] = Encodable.fromValue(value)
  }

  /** Encodable extensions */
  // to avoid conflict between EncodeToBvec and fromValue.
  implicit class EncodableToBvec[E](arg: Encodable[E]) {
    /** Convert to a BitVector */
    def toBitVector(): BitVector = arg.toRaw
  }

  /** Traversable of encodables extensions */
  implicit class TravEncodableToBVec(arg: Traversable[Encodable[_]]) {
    /** Convert to a BitVector */
    def toBitVector(): BitVector = arg.map(_.toRaw).toBitVector
  }

  /** Iterator of encodables extensions */
  implicit class ItEncodableToBVec(arg: Iterator[Encodable[_]]) {
    /** Convert to a BitVector */
    def toBitVector(): BitVector = arg.map(_.toRaw).toBitVector
  }
}
