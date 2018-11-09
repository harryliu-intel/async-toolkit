//scalastyle:off magic.number
//scalastyle:off
// TODO create scalastyle config for test
package com.intel.cg.hpfd.madisonbay

import com.intel.cg.hpfd.madisonbay.Encode.encode
import org.scalatest.{WordSpec, Inspectors, Matchers}
import org.scalatest.Inspectors.{forAll => _}
import org.scalatest.prop._
import org.scalatest.prop.Configuration.{PropertyCheckConfigParam, PropertyCheckConfig}
import org.scalacheck.{Arbitrary, Gen, Prop, Shrink}
import org.scalacheck.Arbitrary._

import scala.reflect.{ClassTag, classTag}

import BitVector._
import Encode._
import Encodable._

class BitVectorTest extends CommonSpec {
  import BitVectorTest._
  import BitVectorTest.Extensions._
  import EncodeTest._  // for arbitraries, generators etc.


  "BitVector" can {
    "be built" when given {
      "no arguments (empty)" in {
        val vec0 = BitVector.empty
        val vec1 = BitVector()
        val vec2 = BitVector.zeros(0)
        vec0 should be ('empty)
        vec1 should equal (vec0)
        vec2 should equal (vec0)
      }

      "length" in {
        forAll { (n: Byte) =>
          whenever (n >= 0) {
            val vec0 = BitVector ofLength n
            val vec1 = BitVector.zeros(n)
            val vec2 = BitVector.ones(n)
            vec0 should have length n
            vec1 should have length n
            vec2 should have length n
            forEvery (0 until n/8) { i =>
              vec0.extract[Byte](i * 8) should equal(0.toByte)
              vec1.extract[Byte](i * 8) should equal(0.toByte)
              vec2.extract[Byte](i * 8) should equal((~0).toByte)
            }
          }
        }
      }

      "long" in {
        forAll { (n: Long) =>
          val vec = BitVector(n)
          vec should have length 64
          vec.toLong should equal (n)
          vec.extract[Long] should equal (n)
        }
      }

      "list of longs" in {
        forAll { (li: List[Long]) =>
          val vec = BitVector(li)
          vec should have length (li.length * 64)
          forEvery (li.zipWithIndex) { case (el, idx) =>
            vec.extract[Long](idx * 64) should equal (el)
          }
        }
      }

      "traversable of longs" in {
        forAll { (li: Iterable[Long]) => // Iterable for zipWithIndex
          val vec = BitVector(li)
          forEvery (li.zipWithIndex) { case (el, idx) =>
            vec.extract[Long](idx * 64) should equal (el)
          }
        }
      }

      "iterator of longs" in {
        forAll { (li: Iterable[Long]) =>
          val vec = BitVector(li.iterator)
          forEvery(li.zipWithIndex) { case (el, idx) =>
            vec.extract[Long](idx * 64) should equal(el)
          }
        }
      }

      "string in binary format" in {
        forAll { (n: Long) =>
          val str = n.toBinaryString.reverse.toString
          val vec = BitVector.fromBin(str)
          vec.toLong should equal (n)
        }
      }

      "string in hexal format" in {
        forAll { (n: Long) =>
          val str = n.toHexString.reverse.toString
          val vec = BitVector.fromHex(str)
          vec.toLong should equal (n)
        }
      }

      "string interpolation" should handle {
        "binary  literals" in {
          bvec"0".toLong should equal (0L)
          bvec"00000000".toLong should equal (0L)
          bvec"10110000".toLong should equal (13L)
          bvec"11111111".toLong should equal (255L)
          xvec"ff".toLong should equal (255L)
        }

        "homogenous encodables" in {
          forAll { (n: Long) =>
            bvec"$n".toLong should equal(n)
          }
          forAll { (n1: Byte, n2: Byte) =>
            bvec"$n1$n2" should equal(BitVector() :+ n1 :+ n2)
            bvec"$n1$n2".toLong should equal(n1.toUlong + 256 * n2.toUlong)
          }
          forAll { (n1: Byte, n2: Byte, n3: Byte) =>
            bvec"$n1$n2$n3" should equal(BitVector() :+ n1 :+ n2 :+ n3)
            bvec"$n1$n2$n3".toLong should equal(n1.toUlong + (n2.toUlong << 8) + (n3.toUlong << 16))
          }
        }

        "heterogenous encodables" in {
          forAll { (n1: Byte, n2: Int, n3: Byte) =>
            bvec"$n1$n2$n3" should equal (BitVector() :+ n1 :+ n2 :+ n3)
            bvec"$n1$n2$n3".toLong should equal (n1.toUlong + (n2.toUlong << 8) + (n3.toUlong << 40) )
          }

          forAll { (n1: Int, n2: Trilean, n3: Long) =>
            val res = bvec"$n1$n2$n3"
            res should equal (BitVector() :+ n1 :+ n2 :+ n3)
            res.toLong should equal (n1.toUlong + (n2.toUlong << 32) + (n3 << 34) )
            (res << 64).toLong should equal (n3 >>> 30)
          }
        }

        "mixing encodables and literals" in {
          forAll { (n1: Int, n2: Byte) =>
            val res = bvec"${n1}01${n2}"
            res should equal ((BitVector() :+ n1) ++ BitVector(2L, length=2) :+ n2)
            res.toLong should equal (n1.toUlong + (1L << 33) + (n2.toUlong << 34))
          }
        }
      }
    } // "be buildable" should

    "be done binary operation" that is {
      byteFunc("negation (~)", ~_, ~_)
      byteBiFunc("conjunction (&)", _ & _, _ & _)
      byteBiFunc("alternative (|)", _ | _, _ | _)
      byteBiFunc("exclusive alternative (^)", _ ^ _, _ ^ _)

      "sum (|) multiple encodables" in {
        forAll ((genEncodableList, "list")) { li =>
          val res = BitVector.empty.sumAll(li: _*)
          val mlen = (0 :: li.map(_.size)).max
          val exp = li.map(_.toRaw).foldLeft(BitVector.empty)(_ | _)
          res should equal (exp)
        }
      }

      "shift right (>>)" in {
        forAll { (vec: BitVector, shift: Byte) =>
          whenever (shift >= 0) {
            val shifted = vec >> shift
            shifted should have length (vec.length + shift)
            forEvery(0 until vec.length / 8) { i =>
              val els = shifted.extract[Byte](8 * i + shift)
              val elv = vec.extract[Byte](8 * i)
              els should equal(elv)
            }
          }
        }
      }

      "shift left  (<<)" in {
        forAll { (vec: BitVector) =>
          val inRange = Gen.choose(0, vec.length)
          val outRange = Gen.choose(vec.length, 2*vec.length)
          forAll ((inRange, "shift")) { shift =>
            val shifted = vec << shift
            shifted should have length (vec.length - shift)
            forEvery(0 until shifted.length / 8) { i =>
              val els = shifted.extract[Byte](8 * i)
              val elv = vec.extract[Byte](8 * i + shift)
              els should equal(elv)
            }
          }
          forAll  ((outRange, "shift")) { shift =>
            val shifted = vec << shift
            shifted should be ('empty)
          }
        }
      }

      "preserving shift right (>>>)" in {
        forAll { (vec: BitVector, shift: Byte) =>
          whenever (shift >= 0) {
            val shifted = vec >>> shift
            shifted should have length (vec.length)
            forEvery(1 to (vec.length-shift-7) / 8) { i =>
              val els = shifted.extract[Byte](vec.length - 8 * i)
              val elv = vec.extract[Byte](vec.length - shift - 8 * i)
              els should equal (elv)
            }
          }
        }
      }

      "preserving shift left  (<<<)" in {
        forAll { (vec: BitVector, shift: Byte) =>
          whenever (shift >= 0) {
            val shifted = vec <<< shift
            shifted should have length (vec.length)
            forEvery(0 until (vec.length-shift-7) / 8) { i =>
              val els = shifted.extract[Byte](8 * i)
              val elv = vec.extract[Byte](shift + 8 * i)
              els should equal (elv)
            }
          }
        }
      }
    }

    "do length operation" that is {
      "pad to length (padTo, podOnesTo)" in {
        forAll { (vec: BitVector) =>
          val inRange = Gen.choose(0, vec.length)
          val outRange = Gen.choose(vec.length, 2*vec.length)

          forAll ((inRange, "length")) { length =>
            val res0 = vec.padTo(length)
            val res1 = vec.padOnesTo(length)
            res0 should equal (vec)
            res1 should equal (vec)
          }
          forAll ((outRange, "length")) { length =>
            val res0 = vec.padTo(length)
            val res1 = vec.padOnesTo(length)
            res0 should have length length
            res1 should have length length
            res0.splitAt(vec.length) should equal ((vec, BitVector.zeros(length - vec.length)))
            res1.splitAt(vec.length) should equal ((vec, BitVector.ones(length - vec.length)))
          }
        }
      }

      "pad to the same length (copad, copadOnes)" in {
        forAll { (vec1: BitVector, vec2: BitVector) =>
          val mlen = vec1.length max vec2.length
          (vec1 copad vec2) should equal ((vec1.padTo(mlen), vec2.padTo(mlen)))
          (vec1 copadOnes vec2) should equal ((vec1.padOnesTo(mlen), vec2.padOnesTo(mlen)))
        }
      }

      "trim to length" in {
        forAll { (vec: BitVector) =>
          val inRange = Gen.choose(0, vec.length)
          val outRange = Gen.choose(vec.length, 2*vec.length)

          forAll ((inRange, "length")) { length =>
            val res = vec.trimTo(length)
            res should have length (length)
            res should equal (vec.slice(0, length))
          }
        }
      }

      "trim to the same length (cotrim)" in {
        forAll { (vec1: BitVector, vec2: BitVector) =>
          val (res1, res2) = vec1 cotrim vec2
          val mlen = vec1.length min vec2.length
          res1 should equal (vec1.trimTo(mlen))
          res2 should equal (vec2.trimTo(mlen))
        }
      }

      "set fixed length (ofLength)" in {
        forAll { (vec: BitVector, number: Byte) =>
          whenever (number >= 0) {
            val res = vec.ofLength(number)
            val mlen = res.length min vec.length
            res.slice(0, mlen) should equal(vec.slice(0, mlen))
          }
        }
      }
    }

    "do container operation" that is {
      "append encodable  (:+)" in {
        forAll { (vec: BitVector, el: Byte) =>
          val res = vec :+ el
          res should have length (vec.length + 8)
          res.extract[Byte](res.length-8) should equal (el)
        }
      }

      "prepend encodable (+:)" in {
        forAll { (vec: BitVector, el: Byte) =>
          val res = el +: vec
          res should have length (vec.length + 8)
          res.extract[Byte](0) should equal (el)
        }
      }

      "concatenate (++)" in {
        forAll { (vec1: BitVector, vec2: BitVector) =>
          val res = vec1 ++ vec2
          res should have length (vec1.length + vec2.length)
          res.slice(0, vec1.length) should equal (vec1)
          res.slice(vec1.length, res.length) should equal (vec2)
          res.splitAt(vec1.length) should equal (vec1, vec2)
        }
      }

      "append multiple encodables" in {
        forAll ((genEncodableList, "list")) { li =>
          val res = BitVector.empty.appendAll(li: _*)
          val no = li.reverse.foldLeft(res)((st, el) => {
            val (nst, rest) = st.pop(el.size)
            rest should equal (el.toRaw)
            nst
          })
          no should equal (BitVector.empty)
        }
      }

      "split (splitAt)" in {
        forAll { (vec: BitVector, number: Byte) =>
          whenever (0 <= number && number < vec.length) {
            val (res1, res2) = vec.splitAt(number)
            (res1.length + res2.length) should equal (vec.length)
            vec.slice(0, res1.length) should equal (res1)
            vec.slice(res1.length, vec.length) should equal (res2)
            (res1 ++ res2) should equal (vec)
          }
        }
      }

      "pop some bits" in {
        forAll { (vec: BitVector, number: Byte) =>
          whenever (0 <= number && number < vec.length) {
            val (res1, res2) = vec.pop(number)
            val (exp1, exp2) = vec.splitAt(vec.length - number)
            res1 should equal (exp1)
            res2 should equal (exp2)
          }
        }
      }

      "pop an encodable" in {
        forAll { (vec: BitVector, el: Byte) =>
          val rvec = vec :+ el
          val (rest, rel) = rvec.pop[Byte]
          rest should equal (vec)
          rel should equal (el)
        }
      }

      "take some bits" in {
        forAll { (vec: BitVector, number: Byte) =>
          whenever (0 <= number && number < vec.length) {
            val res = vec.take(number)
            val (exp, _) = vec.splitAt(number)
            res should equal (exp)
          }
        }
      }

      "drop some bits" in {
        forAll { (vec: BitVector, number: Byte) =>
          whenever (0 <= number && number < vec.length) {
            val res = vec.drop(number)
            val (_, exp) = vec.splitAt(number)
            res should equal (exp)
          }
        }
      }

      "slice/extract range" in {
        forAll { (vec: BitVector) =>
          val inRange = Gen.choose(0, vec.length)
          //TODO: two args as generic params?
          forAll (inRange, inRange) { (n1: Int, n2: Int) =>
            val (start, end) = (n1 min n2, n1 max n2)
            val res = vec.slice(start, end)
            res should have length (end - start)
            res should equal(vec.take(end).drop(start))
          }
        }
      }
    }

    "do extraction" that is {
      "bytes from longs" in {
        forAll { (li: List[Long]) =>
          val vec = li.toBitVector
          forEvery (0 until 8*li.length) { i =>
            val el = (li(i / 8) >>> ((i%8) * 8)).toByte
            vec.extract[Byte](8 * i) should equal (el)
          }
        }
      }

      "booleans (bits) from bytes" in {
        forAll { (li: List[Byte]) =>
          val vec = li.toBitVector
          forEvery (0 until 8*li.length) { i =>
            val el = (li(i / 8) & (1.toByte << (i % 8))) != 0.toByte
            vec.extract[Boolean](i) should equal (el)
          }
        }
      }

      "longs from bytes" in {
        forAll { (li: List[Byte]) =>
          val vec = li.toBitVector
          forEvery (0 until li.length/8) { i =>
            val el = (0 until 8).map(j => (li(i*8 + j).toUlong << (j*8))).reduce(_ | _)
            vec.extract[Long](64 * i) should equal (el)
          }
        }
      }
    }

    "do update" that is {
      "bytes of longs" in {
        forAll { (li: List[Long]) =>
          val vec = li.toBitVector
          forEvery (0 until 8*li.length) { i =>
            val el = (li(i / 8) >>> ((i%8) * 8)).toByte
            val rel = (~el).toByte
            val res = vec.updated(8 * i, rel)
            res.extract[Byte](8 * i) should equal (rel)
          }
        }
      }

      "booleans (bits) of bytes" in {
        forAll { (li: List[Byte]) =>
          val vec = li.toBitVector
          forEvery (0 until 8*li.length) { i =>
            val el = (li(i / 8) & (1.toByte << (i % 8))) != 0.toByte
            val rel = !el
            val res = vec.updated(i, rel)
            res.extract[Boolean](i) should equal (rel)
          }
        }
      }

      "longs of bytes" in {
        forAll { (li: List[Byte]) =>
          val vec = li.toBitVector
          forEvery (0 until li.length/8) { i =>
            val el = (0 until 8).map(j => (li(i*8 + j).toUlong << (j*8))).reduce(_ | _)
            val rel = ~el
            val res = vec.updated(64 * i, rel)
            res.extract[Long](64 * i) should equal (rel)
          }
        }
      }
    }
  }

  "Traversable of BitVector" can {
    "be flattened (toBitVector)" in {
      forAll { (li: Traversable[BitVector]) =>
        val res = li.toBitVector
        val slen = li.map(_.length).foldLeft(0)(_ + _)
        res should have length slen
        li.foldLeft(res) { case (st, el) =>
          val (rel, rest) = st.splitAt(el.length)
          rel should equal (el)
          rest
        }
      }
    }
  }

  "Iterator of BitVector" can {
    "be flattened (toBitVector)" in {
      forAll { (li: Iterable[BitVector]) =>
        val res = li.iterator.toBitVector
        val slen = li.map(_.length).foldLeft(0)(_ + _)
        res should have length slen
        li.foldLeft(res) { case (st, el) =>
          val (rel, rest) = st.splitAt(el.length)
          rel should equal (el)
          rest
        }
      }
    }
  }


  private def byteFunc(name: String, fbvec: BitVector => BitVector, fbyte: Byte => Int) =
    name in {
      forAll { (vec: BitVector) =>
        val res = fbvec(vec)

        res should have length (vec.length)
        forEvery(0 until res.length / 8) { i =>
          val elv = vec.extract[Byte](i * 8)
          val elr = res.extract[Byte](i * 8)
          elr should equal(fbyte(elv).toByte)
        }
      }
    }

  private def byteBiFunc(name: String, fbvec: (BitVector, BitVector) => BitVector, fbyte: (Byte, Byte) => Int) =
    name in {
      forAll { (vec1: BitVector, vec2: BitVector) =>
        val res = fbvec(vec1, vec2)
        val (pvec1, pvec2) = vec1 copad vec2

        res should have length (vec1.length max vec2.length)
        forEvery(0 until res.length / 8) { i =>
          val el1 = pvec1.extract[Byte](i * 8)
          val el2 = pvec2.extract[Byte](i * 8)
          val elr = res.extract[Byte](i * 8)
          elr should equal(fbyte(el1, el2).toByte)
        }
      }
    }
}

object BitVectorTest {
  val genBinDigit: Gen[Char] = Gen.choose('0', '1')
  val genBitVector: Gen[BitVector] = Gen.listOf(genBinDigit).map(x => BitVector.fromBin(x.mkString))

  implicit lazy val arbBitVector: Arbitrary[BitVector] = Arbitrary(genBitVector)

  object Extensions {
    implicit class Trilean2Ulong(n: Trilean) {
      def toUlong(): Long = encode[Trilean].toRaw(n).toLong
    }

    implicit class Byte2Ulong(n: Byte) {
      def toUlong(): Long = 0xffL & n
    }

    implicit class Short2Ulong(n: Short) {
      def toUlong(): Long = 0xffffL & n
    }

    implicit class Int2Ulong(n: Int) {
      def toUlong(): Long = 0xffffffffL & n
    }
  }
}