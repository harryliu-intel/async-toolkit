//scalastyle:off magic.number
//scalastyle:off
// TODO create scalastyle config for test
package com.intel.cg.hpfd.madisonbay

import org.scalatest.{WordSpec, Inspectors, Matchers}
import org.scalatest.Inspectors.{forAll => _}
import org.scalatest.prop._
import org.scalatest.prop.Configuration.{PropertyCheckConfigParam, PropertyCheckConfig}
import org.scalacheck.{Arbitrary, Gen, Prop, Shrink}
import org.scalacheck.Arbitrary._

import scala.reflect.{ClassTag, classTag}

import Encode._
import BitVector._

class EncodeTest extends CommonSpec {
  import EncodeTest._

  "Implementor of Encode" can {
    "be turned into a BitVector" when is {
      en2bvec[Boolean]
      en2bvec[Byte]
      en2bvec[Int]
      en2bvec[Long]
      en2bvec[Trilean]("custom enum (trilean)")
    }
  }

  "Traversable of encodables" can {
    "be turned into a BitVector" when containing {
      trav2bvec[Boolean]
      trav2bvec[Byte]
      trav2bvec[Int]
      trav2bvec[Long]
      trav2bvec[Trilean]("custom enum (trilean)")
    }
  }

  "Iterator of encodables" can {
    "be turned into a BitVector" when containing {
      it2bvec[Boolean]
      it2bvec[Byte]
      it2bvec[Int]
      it2bvec[Long]
      it2bvec[Trilean]("custom enum (trilean)")
    }
  }


  private def en2bvec[E: ClassTag: Encode: Arbitrary](): Unit = {
    val name = classTag[E].runtimeClass.getSimpleName
    en2bvec[E](name)
  }
  private def en2bvec[E: Encode: Arbitrary](name: String): Unit = {
    name in {
      forAll { (n: E) =>
        val vec = n.toBitVector
        vec should have length encode[E].size
        vec.extract[E] should equal(n)
      }
    }
  }

  private def trav2bvec[E: ClassTag: Encode: Arbitrary](): Unit = {
    val name = classTag[E].runtimeClass.getSimpleName
    trav2bvec[E](name)
  }
  private def trav2bvec[E: Encode: Arbitrary](name: String): Unit = {
    name in {
      forAll { (li: Iterable[E]) => // Iterable for zipWithIndex
        val vec = li.toBitVector
        forEvery (li.zipWithIndex) { case (el, idx) =>
          vec.extract[E](idx * encode[E].size) should equal (el)
        }
      }
    }
  }

  private def it2bvec[E: ClassTag: Encode: Arbitrary](): Unit = {
    val name = classTag[E].runtimeClass.getSimpleName
    it2bvec[E](name)
  }
  private def it2bvec[E: Encode: Arbitrary](name: String): Unit = {
    name in {
      forAll { (li: Iterable[E]) => // Iterable for zipWithIndex
        val vec = li.iterator.toBitVector
        forEvery (li.zipWithIndex) { case (el, idx) =>
          vec.extract[E](idx * encode[E].size) should equal (el)
        }
      }
    }
  }
}

object EncodeTest {
  val genEncodable =
    Gen.oneOf[Encodable[_]] (
      arbitrary[Boolean].map(x => Encodable(x)),
      arbitrary[Byte].map(x => Encodable(x)),
      arbitrary[Short].map(x => Encodable(x)),
      arbitrary[Int].map(x => Encodable(x)),
      arbitrary[Long].map(x => Encodable(x)),
      arbitrary[Trilean].map(x => Encodable(x))
    )
  val genEncodableList = Gen.listOf(genEncodable)

  implicit lazy val arbEncodable: Arbitrary[Encodable[_]] = Arbitrary(genEncodable)
  implicit lazy val arbEncodableList: Arbitrary[List[Encodable[_]]] = Arbitrary(genEncodableList)
}