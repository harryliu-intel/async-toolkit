//scalastyle:off
package com.intel.cg.hpfd.madisonbay

import scala.math.{min, max}
import scala.Function.tupled
import shapeless._


/**
  * Bit container of arbitrary length.
  */
class BitVector private(val segs: IndexedSeq[Long], val length: Int) extends Equals { self =>
  import BitVector._
  import Encode._
  import Encodable._

  /*
   * Standard stuff like comparison, hash etc.
   */

  override def canEqual(o: Any): Boolean = o.isInstanceOf[BitVector]
  override def equals(o: Any): Boolean = o match {
    case that: BitVector => {
      that.canEqual(this) &&
        (this.length == that.length) &&
        (this.segs == that.segs)
    }
    case _ => false
  }

  override def hashCode(): Int = segs(0).hashCode()

  /** Stringifies from LEAST significant bits on the left (logical right).
    *
    * The format is: bvec"(bits)"
    * Example:
    * {{{
    *   BitVector(13.toByte).toString == """bvec"10110000""""
    * }}}
    */
  override def toString: String = {
    if (length == 0) {
      s"""bvec"""""
    } else {
      val toBin = (_: Long).toBinaryString.reverse.padTo(64, "0").mkString
      val freeBits = segsLen * 64 - length
      val lastOccBits = ((64 - freeBits) - 1) % 64 + 1

      val str = segs.view.dropRight(1).map(toBin).mkString
      val lastSeg = toBin(segs.last).substring(0, lastOccBits)
      s"""bvec"$str$lastSeg""""
    }
  }


  /*
   * Length-related.
   */

  /** Test whenever this bit vector is empty. */
  def isEmpty(): Boolean = (length == 0)

  /** Number of Long segments. */
  def segsLen: Int = segs.length

  /** Set length. If needed, pad with zeros. */
  def ofLength(length: Int): BitVector = {
    length.compareTo(this.length) match {
      case 0 => this
      case i if i < 0 => take(length)
      case _ /* i > 0 */ => padTo(length)
    }
  }

  /** Pad with zeros to a certain length. */
  def padTo(length: Int): BitVector = {
    if (length <= this.length) {
      this
    } else {
      this | BitVector.zeros(length)  // naive but simple
    }
  }

  /** Pad with ones to a certain length. */
  def padOnesTo(length: Int): BitVector = {
    if (length <= this.length) {
      this
    } else {
      this ++ BitVector.ones(length - this.length)  // naive but simple
    }
  }

  /** Pad two bit vectors so that they're of equal length, filling the shorter with zeros. */
  def copad(other: BitVector): (BitVector, BitVector) = {
    val mlen = this.length max other.length
    (this.padTo(mlen), other.padTo(mlen))
  }

  /** Pad two bit vectors so that they're of equal length, filling the shorter one with ones. */
  def copadOnes(other: BitVector): (BitVector, BitVector) = {
    val mlen = this.length max other.length
    (this.padOnesTo(mlen), other.padOnesTo(mlen))
  }

  /** Trim to a certain length. If it's longer, leave unchanged. */
  def trimTo(length: Int): BitVector = take(length)

  /** Trim two bit vectors so that they're of equal length. */
  def cotrim(other: BitVector): (BitVector, BitVector) = {
    val mlen = this.length min other.length
    (this.trimTo(mlen), other.trimTo(mlen))
  }


  /*
   * Segments access-related.
   */

  /** Safe segs, returns 0 everywhere not occupied */
  protected def liftSegs(i: Int): Long = {
    i match {
      case i if 0 <= i && i < segsLen => segs(i)
      case _ => 0L
    }
  }

  /** Same as shl but shift is guaranteed to be of {{{1 until 64}}}. */
  protected def justShl(segment: Int, shift: Int): Long = {
    val left = liftSegs(segment) << shift
    val right = liftSegs(segment - 1) >>> (64 - shift)
    left | right
  }

  /** Shifted binary left (element-wise right). Occupation-compatible.
    *
    * Used for inline shift right of elements.
    */
  protected def shl(segment: Int, shift: Int): Long = {
    require(shift >= 0)
    val shift0 = shift % 64
    val segment0 = segment - shift / 64
    if (shift0 == 0) {
      liftSegs(segment0)
    } else {
      justShl(segment0, shift0)
    }
  }

  /** Shift right iterator. */
  protected case class ShlIt(shift: Int) extends Iterator[Long] {
    var segment = 0
    val maxSegment = BitVector.segsLen(self.length + shift)
    def hasNext: Boolean = segment < maxSegment
    def next: Long = {
      segment += 1
      shl(segment-1, shift)
    }
  }

  /** Same as shr but shift is guaranteed to be of {{{1 until 64}}}. */
  protected def justShr(segment: Int, shift: Int): Long = {
    val left = liftSegs(segment + 1) << (64 - shift)
    val right = liftSegs(segment) >>> shift
    left | right
  }

  /** Shifted binary right (element-wise left). Occupation-compatible.
    *
    * Occupation-compatible.
    * Loses first `shift` bits.
    */
  protected def shr(segment: Int, shift: Int): Long = {
    require(shift >= 0)
    val shift0 = shift % 64
    val segment0 = segment + shift / 64
    if (shift0 == 0) {
      liftSegs(segment0)
    } else {
      justShr(segment0, shift0)
    }
  }

  /** Shift left iterator. */
  protected case class ShrIt(shift: Int) extends Iterator[Long] {
    var segment = 0
    def hasNext: Boolean = segment < segsLen
    def next: Long = {
      segment += 1
      shr(segment-1, shift)
    }
  }


  /*
   * Binary operations
   */

  /** Pad segs so that they are of the same length. */
  protected def copadSegs(segs1: IndexedSeq[Long], segs2: IndexedSeq[Long]): (IndexedSeq[Long], IndexedSeq[Long]) = {
    val mlen = segs1.length max segs2.length
    (segs1.padTo(mlen, 0L), segs2.padTo(mlen, 0L))  // last segment is already zeroed at unocuppied parts
  }

  /** Combine two BitVectors with Long binary function. */
  protected def combine(other: BitVector, f: (Long, Long) => Long): BitVector = {
    val mlen = this.length max other.length
    val (s1, s2) = copadSegs(this.segs, other.segs)
    val segs = (s1 zip s2).map { case (i, j) => f(i, j) }
    BitVector(segs, mlen)
  }

  /** Bit alternative.
    *
    * Can make the result longer than either of arguments.
    */
  def unary_~(): BitVector = BitVector(segs.map(x => ~x), length)

  /** Bit product.
    *
    * Can make the result longer than either of arguments.
    */
  def &(other: BitVector): BitVector = combine(other, _ & _)

  /** Bit alternative.
    *
    * Can make the result longer than either of arguments.
    */
  def ^(other: BitVector): BitVector = combine(other, _ ^ _)

  /** Bit sum.
    *
    * Can make the result longer than either of arguments.
    */
  def |(other: BitVector): BitVector =  combine(other, _ | _)

  /** Shortcut, x | el == x | el.toBitVector */
  def |[E: Encode](el: E): BitVector = this | el.toBitVector

  /** Shortcut, x | el == x | el.toBitVector */
  def |[E](el: Encodable[E]): BitVector = this | el.toBitVector

  /** Shift right element-wise (changes length).
    *
    * Note: Element-wise is the opposite of bit-wise!
    */
  def >>(shift: Int): BitVector = BitVector(ShlIt(shift), length + shift)

  /** Shift left element-wise (changes length).
    *
    * Note: Element-wise is the opposite of bit-wise!
    */
  def <<(shift: Int): BitVector = {
    if (shift < length) {
      BitVector(ShrIt(shift), length - shift)
    } else {
      BitVector.empty
    }
  }

  /** Shift right element-wise with constant length.
    *
    * Note: Element-wise is the opposite of bit-wise!
    */
  def >>>(shift: Int): BitVector = {
    if (shift > length) {
      BitVector.zeros(length)
    } else {
      val sli = slice(0, length-shift)
      (sli >> shift)
    }
  }

  /** Shift left element-wise with constant length.
    *
    * Note: Element-wise is the opposite of bit-wise!
    */
  def <<<(shift: Int): BitVector = {
    val zeros = BitVector.zeros(length)
    if (shift > length) {
      zeros
    } else {
      val sli = slice(shift, length)
      zeros | sli
    }
  }

  /** Sum all arguments into the vector (union-style). */
  def sumAll(args: Encodable[_]*): BitVector = {
    args.foldLeft(BitVector.empty)(_ | _)
  }


  /*
   * Container-like operations
   */

  /** Concatenate two bit vectors. */
  def ++(other: BitVector): BitVector = this | (other >> this.length)

  /** Concatenate two bit vectors. */
  def ++:(other: BitVector): BitVector = other | (this >> other.length)

  /** Append all elements of a container of encodables. */
  def ++(other: Traversable[Encodable[_]]): BitVector = this ++ other.toBitVector

  /** Prepend all elements of a container of encodables. */
  def ++:(other: Traversable[Encodable[_]]): BitVector = other.toBitVector ++ this

  /** Append encodable value */
  def :+[E: Encode](el: E): BitVector = this ++ el.toBitVector

  /** Append encodable value */
  def :+[E](el: Encodable[E]): BitVector = this ++ el.toBitVector

  /** Prepend encodable value */
  def +:[E: Encode](el: E): BitVector = el.toBitVector ++ this

  /** Prepend encodable value */
  def +:[E](el: Encodable[E]): BitVector = el.toBitVector ++ this

  /** Append all arguments into subsequent positions in the vector (struct-style). */
  def appendAll(args: Encodable[_]*): BitVector = {
    args.foldLeft(BitVector.empty)(_ :+ _)
  }

  /** Append all arguments into subsequent positions in the vector (struct-style). */
  def prependAll(args: Encodable[_]*): BitVector = {
    args.foldLeft(BitVector.empty) { case (st, el) => el +: st }
  }

  /** Splits into two arrays at a certain point.
    *
    * Eg. for ~>011010, splitAt(2) returns (~>01, ~>1010)
    * */
  def splitAt(at: Int): (BitVector, BitVector) = {
    require( at >= 0 )
    require( at <= length )

    pop(length - at)
  }

  /** Pops an array of a certain width */
  def pop(width: Int): (BitVector, BitVector) = {
    require( width >= 0 )
    require( width <= length )

    val lasts = length - width
    (take(lasts), drop(lasts))
  }

  /** Drops a number of elements from the left. */
  def drop(number: Int): BitVector = {
    require( number >= 0 )
    require( number <= length )
    BitVector(ShrIt(number), length - number)
  }

  /** Takes a number of elements from the left. */
  def take(number: Int): BitVector = {
    require( number >= 0 )
    require( number <= length )
    val segNum = BitVector.segsLen(number)
    BitVector(segs.take(segNum), number)
  }

  /** Pops an encodable value */
  def pop[E: Encode]: (BitVector, E) = {
    val (left, right) = pop(encode[E].size)
    val value = encode[E].fromRaw(right)
    (left, value)
  }


  /*
   * Slicings and extractions
   */

  /** Slices the vector into subvector */
  def slice(from: Int, until: Int): BitVector = {
    require( from >= 0 )
    require( from <= until )
    require( until <= length )

    BitVector(ShrIt(from), until - from)
  }

  /** Update a slice */
  def updated(start: Int, value: BitVector): BitVector = {
    require( start >= 0 )
    require( start + value.length <= length )

    take(start) ++ value ++ drop(start + value.length)
  }

  /** Update a slice at range. Vector provided is adjusted. */
  def updated(range: Range, value: BitVector): BitVector = {
    require( range.start >= 0 )
    require( range.last <= length )

    val valRa = value.ofLength(range.last - range.start + 1)
    take(range.start) ++ valRa ++ drop(range.last)
  }

  /** Extracts a subarray */
  def extract(range: Range): BitVector = slice(range.start, range.last+1)

  /** Alias for subarray extraction */
  def apply(range: Range): BitVector = extract(range)

  /** Extract an encodable at position. */
  def extract[E: Encode](pos: Int): E = {
    val width = encode[E].size
    val bits = extract(pos until pos + width)
    encode[E].fromRaw(bits)
  }

  /** Update an encodable at position */
  def updated[E: Encode](start: Int, value: E): BitVector = {
    val bits = encode[E].toRaw(value)
    updated(start, bits)
  }

  /** Update an encodable at position */
  def updated[E](start: Int, en: Encodable[E]): BitVector = updated(start, en.toRaw)

  /** Extract an encodable. */
  def extract[E: Encode]: E = extract[E](0)

  /** Update an encodable */
  def updated[E: Encode](value: E): BitVector = updated(0, value)

  /** Update an encodable */
  def updated[E](en: Encodable[E]): BitVector = updated(0, en)

  /** Extract an encodable at position. */
  def apply[E: Encode](pos: Int): E = extract[E](pos)

  /** Extract an encodable. */
  def apply[E: Encode]: E = extract[E]

  /** Extract with virtual zero padding */
  def padExtract[E: Encode](start: Int): E = padTo(start + encode[E].size).extract[E](start)

  /** Extract with virtual zero padding */
  def padExtract[E: Encode]: E = padExtract[E](0)

  /** Update with virtual zero padding */
  def padUpdate[E: Encode](start: Int, value: E): BitVector = padTo(start + encode[E].size).updated(start, value)

  /** Update with virtual zero padding */
  def padUpdate[E: Encode](value: E): BitVector = padUpdate(0, value)

  /** Update with virtual zero padding */
  def padUpdate[E](start: Int, en: Encodable[E]): BitVector = padTo(start + en.size).updated(start, en)

  /** Update with virtual zero padding */
  def padUpdate[E](en: Encodable[E]): BitVector = padUpdate(0, en)

  /** Extract Boolean */
  def toBoolean: Boolean = padExtract[Boolean]

  /** Extract Byte */
  def toByte: Byte = padExtract[Byte]

  /** Extract Long */
  def toLong: Long = padExtract[Long]

  /** Get raw Long parts. */
  def toRaw: IndexedSeq[Long] = segs
}

/** Companion object to the BitVector class. */
object BitVector {
  /** Number of Long segments for particular length. */
  private def segsLen(length: Int): Int = (length + 63) / 64

  /** From multiple 64 bit values. */
  // basic constructor, ensures zero-ing of unused bits
  def apply(values: IndexedSeq[Long], length: Int): BitVector = {
    require(length >= 0,
      s"BitVector has to have positive length, found: $length")
    require(length <= values.length * 64,
      s"BitVector's length has to be no bigger than its data's bit length, found: $length > ${values.length * 64}")
    require(length > (values.length - 1) * 64,
      s"BitVector's length has to be bigger than its data with one element less, found: $length <= ${(values.length - 1) * 64}")

    if (length % 64 == 0) {
      new BitVector(values, length)
    } else {
      val safeBits = length % 64
      val safeMask = (~0L) >>> (64 - safeBits)
      val safeLast = values.last & safeMask
      val safeValues = values.updated(values.length - 1, safeLast)
      new BitVector(safeValues, length)
    }
  }

  /** From multiple 64 bit values. */
  def apply(values: IndexedSeq[Long]): BitVector = BitVector(values, values.length * 64)

  /** Empty vector. */
  def empty: BitVector = BitVector(IndexedSeq[Long](), 0)

  /** Empty vector. */
  def apply(): BitVector = empty

  /** Uninitialized vector of certain length. */
  def ofLength(length: Int): BitVector = zeros(length)

  /** Bit vector filled with zeros. */
  def zeros(length: Int): BitVector = {
    require( length >= 0, "BitVector must have non-negative length" )
    BitVector(IndexedSeq.fill(segsLen(length))(0L), length)
  }

  /** Bit vector filled with ones. */
  def ones(length: Int): BitVector = {
    require( length >= 0, "BitVector must have non-negative length" )
    BitVector(IndexedSeq.fill(segsLen(length))(~0L), length)
  }

  /** Concatenate multiple BitVectors */
  def concat(args: Traversable[Encodable[_]]*): BitVector = args.map(_.toBitVector).toBitVector

  /** From single 64 bit value and length. */
  def apply(value: Long, length: Int = 64): BitVector = BitVector(IndexedSeq[Long](value), length)


  // List is both Traversable and Product, needs to be listed separately
  /** From 64 bit values list. */
  def apply(values: List[Long]): BitVector = BitVector(values.toIndexedSeq)

  /** From traversable of 64 bit values and length. */
  def apply(values: Traversable[Long], length: Int): BitVector = {
    BitVector(values.takeLen(length).toIndexedSeq, length)
  }

  /** From traversable of 64 bit values. */
  def apply(values: Traversable[Long]): BitVector = BitVector(values.toIndexedSeq)

  /** From iterator of 64 bit values and length. */
  def apply(values: Iterator[Long], length: Int): BitVector = {
    val seq = values.takeLen(length).toIndexedSeq
    BitVector(seq, length)
  }

  /** From iterator of 64 bit values. */
  def apply(values: Iterator[Long]): BitVector = {
    BitVector(values.toIndexedSeq)
  }


  /*
   * String conversions
   */

  /** From binary string representation */
  def fromBin(str: String): BitVector = {
    val state = str.grouped(64).map { ss =>
      ss.zipWithIndex.foldRight(0L) { case ((ch, i), st) =>
        ch match {
          case '1' => st | (1L << i)
          case '0' => st
          case _ => {
            assert(false, s"""BitVector expected binary, encountered character: '$ch' in string "$str"""")
            st
          }
        }
      }
    }
    BitVector(state, str.length)
  }

  /** From hexal string representation.
    *
    * Attention! Order of letters is reversed but the letters themselves should be the same!
    * {{{
    *   BitVector.fromHex("7") == BitVector.fromBinary("111")
    *   BitVector.fromHex("71") == BitVector.fromBinary("11101")
    * }}}
    */
  def fromHex(str: String): BitVector = {
    val hexLetter = "[a-f]".r
    val state = str.grouped(16).map { ss =>
      ss.zipWithIndex.foldRight(0L) { case ((ch, i), st) =>
        ch match {
          case _ if ch.isDigit => st | ((ch.toLong - '0'.toLong) << (i*4))
          case hexLetter() => st | ((ch.toLong - 'a'.toLong + 10L) << (i*4))
          case _ => {
            assert(false, s"""BitVector expected hexal, encountered character: '$ch' in string "$str"""")
            st
          }
        }
      }
    }
    BitVector(state, str.length * 4)
  }

  /** Implicit class for string formatter. */
  implicit class BitVectorStringContext(val sc: StringContext) extends AnyVal {
    private def fromStr(fromStr: String => BitVector, args: Encodable[_]*): BitVector = {
      val vec = sc.parts.zip(args).foldLeft(BitVector.empty) { case (st, (s, a)) =>
        st ++ fromStr(s) ++ a.toRaw
      }
      vec ++ fromStr(sc.parts.last)
    }

    /** Binary interpolation */
    def bvec(args: Encodable[_]*): BitVector = fromStr(BitVector.fromBin _, args: _*)

    /** Hex interpolation */
    def xvec(args: Encodable[_]*): BitVector = fromStr(BitVector.fromHex _, args: _*)
  }


  /*
   * Compositions (unions, structures)
   */

  /** Create union-style bit vector. */
  def union[P <: Product, L <: HList](args: P)
                                     (implicit gen: Generic.Aux[P,L],
                                      el: BitVector.EncodeList[L]): BitVector = {
    el.sumAll(gen.to(args), BitVector.empty)
  }

  /** Create struct-style bit vector. */
  def struct[P <: Product, L <: HList](args: P)
                                      (implicit gen: Generic.Aux[P,L],
                                       el: BitVector.EncodeList[L]): BitVector = {
    el.appendAll((gen.to(args), 0), BitVector.empty)
  }


  protected trait EncodeList[T] {
    def sumSize: Int
    def sumAll(value: T, state: BitVector): BitVector
    def appendAll(tup: (T, Int), state: BitVector): BitVector
  }

  implicit val enLiHNil = new EncodeList[HNil] {
    def sumSize: Int = 0
    def sumAll(value: HNil, state: BitVector): BitVector = state
    def appendAll(tup: (HNil, Int), state: BitVector): BitVector = state
  }
  implicit def enLiHCons[H, T <: HList](implicit en: Encode[H], me: Lazy[EncodeList[T]]): EncodeList[H :: T] =
    new EncodeList[H :: T] {
      def sumSize: Int = (en.size + me.value.sumSize)
      def sumAll(value: H :: T, state: BitVector): BitVector = {
        me.value.sumAll(value.tail, state | value.head)
      }
      def appendAll(tup: (H :: T, Int), state: BitVector): BitVector = {
        val (value, at) = tup
        val newAt = at + en.size
        val newState = state | (en.toRaw(value.head) >> at)
        me.value.appendAll((value.tail, newAt), newState)
      }
    }


  /** takeLen and dropLen for Long traversables */
  protected implicit class LenTraversable(val it: Traversable[Long]) {
    def takeLen(length: Int): Traversable[Long] = it take BitVector.segsLen(length)
    def dropLen(length: Int): Traversable[Long] = it drop BitVector.segsLen(length)
  }

  /** takeLen and dropLen for Long iterators */
  protected implicit class LenIterator(val it: Iterator[Long]) {
    def takeLen(length: Int): Iterator[Long] = it take BitVector.segsLen(length)
    def dropLen(length: Int): Iterator[Long] = it drop BitVector.segsLen(length)
  }


  /*
   * Extension classes for toBitVector
   */

  /** Additional methods for traversables of BitVectors */
  implicit class TravBVec(arg: Traversable[BitVector]) {
    /** Flatten container to a single BitVector. */
    def toBitVector(): BitVector = arg.foldLeft(BitVector.empty)(_ ++ _)
  }

  /** Additional methods for iterators of BitVectors */
  implicit class ItBVec(arg: Iterator[BitVector]) {
    /** Flatten container to a single BitVector. */
    def toBitVector(): BitVector = arg.foldLeft(BitVector.empty)(_ ++ _)
  }
}
