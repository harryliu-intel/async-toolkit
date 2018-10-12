//scalastyle:off
package com.intel.cg.hpfd.madisonbay

import scala.math.{min, max}
import scala.Function.tupled
import shapeless._
import com.intel.cg.hpfd.madisonbay.Encode.encode


/**
  * Bit container of arbitrary length.
  */
class BitVector private(val segs: IndexedSeq[Long], val length: Int) { self =>
  /** Stringifies from LEAST significant bits on the left (logical right).
    *
    * The format is: bvec"(bits)"
    */
  override def toString: String = {
    if (length == 0) {
      s"""bvec"""""
    }
    else {
      val str = (0 until segments - 1).reverse.map(i => segs(i).toBinaryString.reverse.padTo(64, "0").mkString).mkString
      val lastSeg = segs(segments - 1).toBinaryString.reverse.padTo(lastOccBits, "0").mkString.substring(0, lastOccBits)
      s"""bvec"$str$lastSeg""""
    }
  }

  /** Number of Long segments. */
  def segments: Int = segs.length

  /** How many bits from the left (last indexes) are non-significant/free. */
  def freeBits: Int = segments * 64 - length

  /** How many bits from the right (first indexes) in the last segment are significant/occupied. */
  protected def lastOccBits: Int = ((64 - freeBits) - 1) % 64 + 1

  /** Occupation mask over the last segment */
  protected def lastOccMask: Long = (~0L) >> freeBits

  /** Deep copy. Nop, as BitVector is immutable. */
  override def clone: BitVector = this

  /** Safe segs, returns 0 everywhere not occupied */
  protected def occSegs(i: Int): Long = {
    i match {
      case i if i < 0 => 0L
      case i if i < segments - 1 => segs(i)
      case i if i == segments - 1 => segs(i) & lastOccMask
      case _ => 0L
    }
  }

  def canEqual(o: Any): Boolean = o.isInstanceOf[BitVector]
  override def equals(o: Any): Boolean = o match {
    case that: BitVector => {
      that.canEqual(this) &&
        (this.length == that.length) &&
        (0 until length).forall(i => this.occSegs(i) == that.occSegs(i))
    }
    case _ => false
  }

  override def hashCode(): Int = segs(0).hashCode()

  /** Shifted right segment access. Occupation-compatible.
    *
    * Used for inline shift right of elements.
    */
  protected def shl(segment: Int, shift: Int): Long = {
    // require(segment >= 0)
    // require(segment <= segments)  // equal for the last one's overshifted part
    // require(shift >= 0)
    // require(shift < 64)
    if (segment < 0 || segment > segments) {
      0L
    }
    else if (shift == 0) {
      occSegs(segment)
    }
    else {
      val left = if (segment == segments) {
        0L
      } else {
        occSegs(segment) << shift
      }
      val right = if (segment == 0) {
        0L
      } else {
        occSegs(segment - 1) >> (64 - shift)
      }
      left | right
    }
  }

  /** Shift right iterator. */
  protected case class ShlIt(len: Int) extends Iterator[Long] {
    val shift = len % 64
    var segment = len / 64
    def hasNext: Boolean = segment <= segments
    def next: Long = {
      segment += 1
      shl(segment-1, shift)
    }
  }


  /** Shifted left (binary left) segment access.
    *
    * Occupation-compatible.
    * Loses first `shift` bits.
    */
  protected def shr(segment: Int, shift: Int): Long = {
    // require(segment >= 0)
    // require(segment <= segments)  // equal for the last one's overshifted part
    // require(shift >= 0)
    // require(shift < 64)
    if (segment < 0 || segment > segments) {
      0L
    }
    else if (shift == 0) {
      occSegs(segment)
    }
    else {
      val left = if (segment == segments) {
        0L
      } else {
        occSegs(segment) << (64 - shift)
      }
      val right = occSegs(segment) >> shift
      left | right
    }
  }

  /** Concatenate two bit vectors. */
  def ++(other: BitVector): BitVector = {
    // conceptually: this | (other >> this.length)
    if (segments == 0) {
      other
    }
    else {
      val len = length + other.length
      val state =
        if (other.freeBits == 0) { // just copy it too
          segs ++ other.segs
        }
        else {
          val left = segs.dropRight(1)
          val center = segs.last | other.shl(0, 64 - freeBits)

          val resSegments = BitVector.segments(len)
          val rightSegments = resSegments - segments
          val right = ShlIt(64 - freeBits).drop(1).take(rightSegments)

          (left :+ center) ++ right
        }
      BitVector(state, len)
    }
  }

  /** Shift left iterator. */
  protected case class ShrIt(len: Int) extends Iterator[Long] {
    val shift = len % 64
    var segment = len / 64
    def hasNext: Boolean = segment < segments
    def next: Long = {
      segment += 1
      shr(segment-1, shift)
    }
  }

  /** Add (append) encodable value */
  def +[E: Encode](el: E): BitVector = this ++ BitVector(el)

  /** Append encodable value */
  def :+[E: Encode](el: E): BitVector = this ++ BitVector(el)

  /** Prepend encodable value */
  def +:[E: Encode](el: E): BitVector = BitVector(el) ++ this

  /** Zips two seqs padding the shortest one. */
  protected def zipLongest(left: IndexedSeq[Long],
                           right: IndexedSeq[Long],
                           el0: Long = 0L): IndexedSeq[(Long, Long)] = {
    val maxLength = max(left.length, right.length)
    val leftPad = left.padTo(maxLength, el0)
    val rightPad = right.padTo(maxLength, el0)
    leftPad zip rightPad
  }

  /** Combinator helper for two sequences. */
  protected implicit class Combinator(val left: IndexedSeq[Long]) {
    def combine(right: IndexedSeq[Long])(el0: Long = 0L)(fn: (Long, Long) => Long): IndexedSeq[Long] = {
      zipLongest(left, right, el0) map tupled(fn)
    }
    def <|>(right: IndexedSeq[Long]): IndexedSeq[Long] = combine(right)(0L)(_ | _)
    def <^>(right: IndexedSeq[Long]): IndexedSeq[Long] = combine(right)(0L)(_ ^ _)
    def <&>(right: IndexedSeq[Long]): IndexedSeq[Long] = combine(right)(1L)(_ & _)
  }

  protected def combinator(fn: (IndexedSeq[Long], IndexedSeq[Long]) => IndexedSeq[Long]): (BitVector, BitVector) => BitVector = {
    (left: BitVector, right: BitVector) => {
      val len = max(left.length, right.length)
      val state = fn(left.segs, right.segs)
      BitVector(state, len)
    }
  }

  /** Bit alternative.
    *
    * Can make the result longer than either of arguments.
    */
  def unary_~(): BitVector = new BitVector(segs.map(x => ~x), length)

  /** Bit product.
    *
    * Can make the result longer than either of arguments.
    */
  def &(other: BitVector): BitVector = combinator(_ <&> _)(this, other)

  /** Bit alternative.
    *
    * Can make the result longer than either of arguments.
    */
  def ^(other: BitVector): BitVector = combinator(_ <^> _)(this, other)

  /** Bit sum.
    *
    * Can make the result longer than either of arguments.
    */
  def |(other: BitVector): BitVector = combinator(_ <|> _)(this, other)

  /** Shortcut, x | (y, offset) == x | (y >> offset) */
  def |(other: BitVector, offset: Int): BitVector = this | (other >> offset)

  /** Shortcut, x | el == x | BitVector(el) */
  def |[E: Encode](el: E): BitVector = this | BitVector(el)

  /** Shortcut, x | (y, offset) == x | (y >> offset) */
  def |[E: Encode](el: E, offset: Int): BitVector = this | (BitVector(el) >> offset)

  /** Shift right element-wise.
    *
    * Note: Element-wise is the opposite of bit-wise!
    */
  def >>(shift: Int): BitVector = BitVector(ShlIt(shift), length + shift)

  /** Shift left element-wise.
    *
    * Note: Element-wise is the opposite of bit-wise!
    */
  def <<(shift: Int): BitVector = BitVector(ShrIt(shift), length - shift)

  /** Rotate right element-wise.
    *
    * Note: Element-wise is the opposite of bit-wise!
    */
  def >>>(shift: Int): BitVector = {
    val (left, right) = splitAt(length - shift)
    right ++ left
  }

  /** Rotate left element-wise.
    *
    * Note: Element-wise is the opposite of bit-wise!
    */
  def <<<(shift: Int): BitVector = {
    val (left, right) = splitAt(shift)
    right ++ left
  }



  /** Sum all arguments into the vector (union-style). */
  def sumAll[P <: Product, L <: HList](args: P)
                                      (implicit gen: Generic.Aux[P,L],
                                       el: BitVector.EncodeList[L]): BitVector = {
    el.sumAll(gen.to(args), this)
  }

  /** Append all arguments into subsequent positions in the vector (struct-style). */
  def appendAll[P <: Product, L <: HList](args: P)
                                         (implicit gen: Generic.Aux[P,L],
                                          el: BitVector.EncodeList[L]): BitVector = {
    el.appendAll((gen.to(args), 0), this)
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
  def drop(number: Int): BitVector = BitVector(ShrIt(number), length - number)

  /** Takes a number of elements from the left. */
  def take(number: Int): BitVector = BitVector(segs, number)

  /** Pops an encodable value */
  def pop[E: Encode]: (BitVector, E) = {
    val (left, right) = pop(encode[E].size)
    val value = encode[E].fromRaw(right)
    (left, value)
  }

  /** Slices the vector into subvector */
  def slice(from: Int, until: Int): BitVector = {
    require( from >= 0 )
    require( from <= until )
    require( until <= length )

    BitVector(ShrIt(from), until - from)
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

  /** Extract an encodable. */
  def extract[E: Encode]: E = extract[E](0)

  /** Extract an encodable at position. */
  def apply[E: Encode](pos: Int): E = extract[E](pos)

  /** Extract an encodable. */
  def apply[E: Encode]: E = extract[E]

  /** Get raw Long parts. */
  def toRaw: IndexedSeq[Long] = segs
}

object BitVector {
  /** Number of Long segments for particular length. */
  def segments(length: Int): Int = (length + 63) / 64

  /** Full segments and shift. */
  def segShift(length: Int): (Int, Int) = (length / 64, length % 64)

  /** Empty vector. */
  def empty: BitVector = new BitVector(IndexedSeq[Long](), 0)

  /** Uninitialized vector of certain length. */
  def ofLength(length: Int): BitVector = new BitVector(IndexedSeq.fill(segments(length))(0L), length)

  /** Bit vector filled with zeros. */
  def zeros(length: Int): BitVector = new BitVector(IndexedSeq.fill(segments(length))(0L), length)

  /** Bit vector filled with ones. */
  def ones(length: Int): BitVector = new BitVector(IndexedSeq.fill(segments(length))(~0L), length)

  /** From single 64 bit value and length. */
  def apply(value: Long, length: Int = 64): BitVector = new BitVector(IndexedSeq[Long](value), length)

  /** From multiple 64 bit values. */
  def apply(values: IndexedSeq[Long]): BitVector = new BitVector(values, values.length * 64)

  /** From traversable of 64 bit values and length. */
  def apply(values: Traversable[Long], length: Int): BitVector = {
    new BitVector(values.takeLen(length).toIndexedSeq, length)
  }

  /** From traversable of 64 bit values. */
  def apply(values: Traversable[Long]): BitVector = {
    val state = values.toIndexedSeq
    new BitVector(state, state.length * 64)
  }

  /** From iterator of 64 bit values and length. */
  def apply(values: Iterator[Long], length: Int): BitVector = {
    new BitVector(values.takeLen(length).toIndexedSeq, length)
  }

  /** From iterator of 64 bit values. */
  def apply(values: Iterator[Long]): BitVector = {
    val state = values.toIndexedSeq
    new BitVector(state, state.length * 64)
  }

  /** Implicit class for binary string formatter. */
  implicit class BitVectorStringContext(val sc: StringContext) extends AnyVal {
    def bvec(): BitVector = {
      require( sc.parts.length == 1 )
      val part = sc.parts(0)
      val length = part.length
      val segs = BitVector.segments(part.length)
      val state = (0 until segs) map { i =>
        val bits = (min(length, (i+1) * 64) - 1) % 64 + 1
        (0 until bits).foldRight(0L: Long) { case (j, s) =>
          part(i * 64 + j) match {
            case '1' => s | (1L << j)
            case '0' => s
            case x => { assert(false, s"BitVector stores bits only, encountered character: '$x'"); s }
          }
        }
      }
      new BitVector(state, length)
    }
  }

  /** From value that has an encoding to Long. */
  def apply[E: Encode](value: E): BitVector = encode[E].toRaw(value)

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

  /** From multiple values that have encodings to Long. */
  def apply[P <: Product, L <: HList](args: P)
                                     (implicit gen: Generic.Aux[P,L],
                                      el: BitVector.EncodeList[L]): BitVector = {
    struct(args)
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
        val newState = state | (BitVector(value.head), at)
        me.value.appendAll((value.tail, newAt), newState)
      }
    }


  /** takeLen and dropLen for Long traversables */
  protected implicit class LenTraversable(val it: Traversable[Long]) {
    def takeLen(length: Int): Traversable[Long] = it take BitVector.segments(length)
    def dropLen(length: Int): Traversable[Long] = it drop BitVector.segments(length)
  }

  /** takeLen and dropLen for Long iterators */
  protected implicit class LenIterator(val it: Iterator[Long]) {
    def takeLen(length: Int): Iterator[Long] = it take BitVector.segments(length)
    def dropLen(length: Int): Iterator[Long] = it drop BitVector.segments(length)
  }
}
