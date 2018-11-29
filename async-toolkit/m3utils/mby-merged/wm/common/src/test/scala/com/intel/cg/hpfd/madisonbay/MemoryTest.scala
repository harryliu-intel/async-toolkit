//scalastyle:off magic.number
// TODO create scalastyle config for test
package com.intel.cg.hpfd.madisonbay

import com.intel.cg.hpfd.madisonbay.Memory.{Address, AddressRange, Alignment, Bits, Bytes}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.OptionValues._
import org.scalacheck.Prop
import org.scalatest.prop.Checkers

import com.intel.cg.hpfd.madisonbay.Encode.encode
import org.scalatest.Inspectors.{forAll => _}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._

import scala.math._

class MemoryTest extends CommonSpec {

  val genBits: Gen[Bits] = arbitrary[Long].map(Bits.apply _)
  implicit lazy val arbBits: Arbitrary[Bits] = Arbitrary(genBits)

  def pow2(n: Int): Long = (0 to n).foldLeft(1L) { (st, _) => st*2 }


  "Bits" can {
    "toBits (to self)" in {
      forAll { (n: Long) =>
        Bits(n).toBits should equal (Bits(n))
      }
    }

    "toLong" in {
      forAll { (n: Long) =>
        Bits(n).toLong should equal (n)
      }
    }

    "tryBytes" should {
      "for full bytes, return Some" in {
        forAll { (n: Long) =>
          whenever (Long.MinValue / 8 < n && n < Long.MaxValue  / 8) {
            Bits(n * 8).tryBytes.value should equal (Bytes(n))
          }
        }
      }

      "otherwise, return None" in {
        forAll { (n: Long) =>
          whenever (n % 8 != 0) {
            Bits(n).tryBytes should be (empty)
          }
        }
      }
    }

    "fullBytes" should {
      "given full bytes, return them" in {
        forAll { (n: Long) =>
          whenever (Long.MinValue / 8 < n && n < Long.MaxValue  / 8) {
            Bits(n * 8).fullBytes should equal (Bytes(n))
          }
        }
      }

      "otherwise, return closest lower number of full bytes" in {
        forAll { (n: Long) =>
          whenever (Long.MinValue / 8 < n && n < Long.MaxValue  / 8) {
            forAll ((Gen.choose(1, 7), "x")) { x =>
              val nx = n * 8 + signum(n) * x
              Bits(nx).fullBytes should equal(Bytes(n))
            }
          }
        }
      }
    }

    "isPower" should {
      "given power of 2, return true" in {
        forAll ((Gen.choose(1, 60), "n")) { n =>
          val p = pow2(n)
          Bits(p).isPower should equal (true)
        }
      }

      "otherwise, return false" in {
        forAll ((Gen.choose(1, 60), "n")) { n =>
          val p = pow2(n)
          forAll ((Gen.choose(p / 2 + 1, p - 1), "p0")) { p0 =>
            Bits(p0).isPower should equal (false)
          }
        }
      }
    }

    "nextPower" should {
      "given power of 2, return itself" in {
        forAll ((Gen.choose(1, 60), "n")) { n =>
          val p = pow2(n)
          Bits(p).nextPower should equal (Bits(p))
        }
      }

      "otherwise, return closest higher power of 2" in {
        forAll ((Gen.choose(2, 60), "n")) { n =>
          val p = pow2(n)
          forAll ((Gen.choose(p / 2 + 1, p - 1), "p0")) { p0 =>
            Bits(p0).nextPower should equal (Bits(p))
          }
        }
      }
    }

    "be summed (+)" in {
      forAll { (n: Int, m: Int) =>
        (Bits(n) + Bits(m)) should equal (Bits(n.toLong + m.toLong))
      }
    }

    "be subtracted (-)" in {
      forAll { (n: Int, m: Int) =>
        (Bits(n) - Bits(m)) should equal (Bits(n.toLong - m.toLong))
      }
    }

    "be divided (/)" should {
      "when denominator is not zero, return integer" in {
        forAll { (n: Long, m: Long) =>
          whenever(m != 0) {
            (Bits(n) / Bits(m)) should equal (n / m)
          }
        }
      }

      "otherwise, throw ArithmeticException" in {
        forAll { (n: Long) =>
          an [ArithmeticException] should be thrownBy Bits(n) / Bits(0)
        }
      }
    }

    "produce a shift (%)" which {
      "when denominator is not zero, is Bits" in {
        forAll { (n: Long, m: Long) =>
          whenever(m != 0) {
            (Bits(n) % Bits(m)) should equal (Bits(n % m))
          }
        }
      }

      "otherwise, throws ArithmeticException instead" in {
        forAll { (n: Long) =>
          an [ArithmeticException] should be thrownBy Bits(n) % Bits(0)
        }
      }
    }

    "be compared (as numbers)" in {
      forAll { (n: Long, m: Long) =>
        Bits(n) compare Bits(m) should equal (n compare m)
      }
    }
  }

  "Bytes" can {
    "tryAlignment" which {
      "given power of 2, returns Some[Alignment]" in {
        forAll ((Gen.choose(1, 60), "n")) { n =>
          val p = pow2(n)
          Bytes(p).tryAlignment.value should equal (Alignment(p))
        }
      }
    }

    "otherwise, return None" in {
      forAll ((Gen.choose(2, 60), "n")) { n =>
        val p = pow2(n)
        forAll ((Gen.choose(p / 2 + 1, p - 1), "p0")) { p0 =>
          Bytes(p0).tryAlignment should be (empty)
        }
      }
    }
  }

  "Address" can {
    "bits (offset from full words)" in {
      forAll { (n: Long) =>
        whenever (n >= 0) {
          Address(Bits(n)).bits should equal(Bits(n % 64))
        }
      }
    }

    "toBits (number of all owned bits)" in {
      forAll { (n: Long) =>
        whenever (n >= 0) {
          Address(Bits(n)).toBits should equal (Bits(n))
        }
      }
    }

    "be done shift (+)" which is {
      "by Bits" in {
        forAll { (n: Int, m: Int) =>
          whenever (n >= 0 && n.toLong + m.toLong >= 0) {
            val shifted = Address(Bits(n)) + Bits(m)
            val result = Address(Bits(n.toLong + m.toLong))
            shifted should equal (result)
          }
        }
      }

      "by Bytes" in {
        forAll { (n: Int, m: Int) =>
          whenever (n >= 0 && n.toLong + m.toLong * 8 >= 0) {
            val shifted = Address(Bits(n)) + Bytes(m)
            val result = Address(Bits(n.toLong + m.toLong * 8))
            shifted should equal (result)
          }
        }
      }
    }

    "be done reverse shift (-)" which is  {
      "by Bits" in {
        forAll { (n: Int, m: Int) =>
          whenever (n >= 0 && n.toLong - m.toLong >= 0) {
            val shifted = Address(Bits(n)) - Bits(m)
            val result = Address(Bits(n.toLong - m.toLong))
            shifted should equal (result)
          }
        }
      }

      "by Bytes" in {
        forAll { (n: Int, m: Int) =>
          whenever (n >= 0 && n.toLong - m.toLong * 8 >= 0) {
            val shifted = Address(Bits(n)) - Bytes(m)
            val result = Address(Bits(n.toLong - m.toLong * 8))
            shifted should equal (result)
          }
        }
      }
    }


    "produce a shift (%)" which {
      "when denominator is not zero, is Bits" in {
        forAll { (n: Long, m: Long) =>
          whenever (n >= 0 && m != 0) {
            (Address(Bits(n)) % Bits(m)) should equal (Bits(n % m))
          }
        }
      }

      "otherwise, throws ArithmeticException instead" in {
        forAll { (n: Long) =>
          whenever (n >= 0) {
            an[ArithmeticException] should be thrownBy Address(Bits(n)) % Bits(0)
          }
        }
      }
    }

    "be aligned to Alignment" in {
      forAll ((arbitrary[Int], "n"), (Gen.choose(1, 57), "m")) { (n, m) =>
        whenever (n >= 0) {
          val p = pow2(m)
          val al = Alignment(p)
          val p0 = p * 8
          val res = if (n % p0 == 0) { n } else { n + (p0 - n % p0) }
          (Address(Bits(n)) alignTo al) should equal (Address(res))
        }
      }
    }

    "AddressRange" should {
      "fail when created with range <= 0" in {
        forAll { (n: Long, m: Long) =>
          whenever (n >= 0 && m <= 0) {
            an[IllegalArgumentException] should be thrownBy AddressRange(Address(Bits(n)), Bits(m))
          }
        }
      }

      "fail when created with first address > one-after-last address" in {
        forAll { (n: Int, m: Int) =>
          whenever (n >= 0 && m > 0) {
            an [IllegalArgumentException] should be thrownBy AddressRange(Address(Bits(n+m)), Address(Bits(n)))
          }
        }
      }

      "by created via placeReg (properly aligned range)" in {
        forAll ((arbitrary[Int], "n"), (Gen.choose(1, 57), "m")) { (n, m) =>
          whenever (n >= 0) {
            val p = pow2(m)
            val addr = Address(Bits(n))
            val al = Alignment(p)
            val pos = addr alignTo al
            val width = Bits(8 * p)
            AddressRange.placeReg(addr, al) should equal (AddressRange(pos, width))
          }
        }
      }
    }
  }
}
