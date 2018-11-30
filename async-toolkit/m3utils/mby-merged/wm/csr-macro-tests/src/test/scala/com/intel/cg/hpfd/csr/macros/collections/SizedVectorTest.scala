//scalastyle:off magic.number
// TODO create scalastyle config for test
package com.intel.cg.hpfd.csr.macros.collections

import org.scalatest.{WordSpec, Inspectors, Matchers}
import org.scalatest.Inspectors.{forAll => _}
import org.scalatest.prop._
import org.scalatest.prop.Configuration.{PropertyCheckConfigParam, PropertyCheckConfig}

import SizedVector._
import SizedVector.auto._


class SizedVectorTest extends WordSpec with Matchers with Checkers with Inspectors {
  import SizedVectorTest._

  val is = afterWord("is")

  "SizedVector" when {
    "empty" should {
      "be buildable from empty argument list" in {
        SizedVector[Int]() should equal (SizedVector.empty[Int])
      }
      "be empty" in {
        SizedVector.empty[Int] should be (empty)
      }
      "have zero length" in {
        SizedVector.empty[Int] should have length 0
      }
    }

    "full" should {
      "be buildable from statically known number of elements" in {
        val vec = SizedVector(3, 1, 4, 1, 5)
        vec should have length 5
      }

      val data = Vector(3, 1, 4, 1, 5)
      val vec = SizedVector(3, 1, 4, 1, 5)

      "have length matching its data" in {
        vec should have length (data.length)
      }

      "provide reading access" which is {
        "statical" that {
          "succeeds when in bounds" in {
            vec(0) should equal (data(0))
            vec(1) should equal (data(1))
            vec(2) should equal (data(2))
            vec(3) should equal (data(3))
            vec(4) should equal (data(4))
          }

          "not compiles when out of bounds" in {
            "vec(-1)" shouldNot compile
            "vec(5)" shouldNot compile
          }
        }

        "dynamical" that {
          "succeeds when in bounds" in {
            forEvery (0 until vec.length) { i =>
              vec.applyUnsafe(i) should equal (data(i))
            }
          }

          "throws a runtime error when out of bounds" in {
            an [IndexOutOfBoundsException] should be thrownBy {
              vec.applyUnsafe(-1)
            }
            an [IndexOutOfBoundsException] should be thrownBy {
              vec.applyUnsafe(vec.length)
            }
          }
        }
      }

      "provide indexing lens" which is {
        "statical" that {
          "succeeds when in bounds" in {
            vec.index(0).get(vec) should equal (data(0))
            vec.index(1).get(vec) should equal (data(1))
            vec.index(2).get(vec) should equal (data(2))
            vec.index(3).get(vec) should equal (data(3))
            vec.index(4).get(vec) should equal (data(4))

            vec.index(0).set(17)(vec) should equal (vec.updated(0, 17))
          }

          "not compiles when out of bounds" in {
            "vec.index(-1)" shouldNot compile
            "vec.index(5)" shouldNot compile
          }
        }

        "dynamical" that {
          "succeeds when in bounds in" in {
            forEvery (0 until vec.length) { i =>
              vec.indexUnsafe(i).get(vec) should equal (data(i))
            }
          }

          "throws a runtime error when out of bounds" in {
            an [IndexOutOfBoundsException] should be thrownBy {
              vec.indexUnsafe(-1)
            }
            an [IndexOutOfBoundsException] should be thrownBy {
              vec.indexUnsafe(vec.length)
            }
          }
        }
      }
    }

    "having single element" should {
      val vec = SizedVector(13)

      "be convertible to scalar" in {
        vec.toScalar should equal (vec(0))
      }
    }

    "not having single element" should {
      val empty = SizedVector.empty[Int]
      val full = SizedVector(2, 7, 2)

      "not compile an attempt to convert to scalar" in {
        "empty.toScalar" shouldNot compile
        "full.toScalar" shouldNot compile
      }
    }
  }

  "Vector" should {
    "have \"sized\" conversion" that {
      "succeeds when sizes match" in {
        val result = SizedVector(3, 1, 4)
        Vector(3, 1, 4).toSized(3) should equal (result)
      }

      "fails when sizes differ" in {
        an [IllegalArgumentException] should be thrownBy {
          Vector(3, 1, 4).toSized(2)
        }
        an [IllegalArgumentException] should be thrownBy {
          Vector(3, 1, 4).toSized(4)
        }
      }

      "not compiles when provided a runtime size" in {
        val s = 3
        "Vector(3, 1, 4).toSized(s)" shouldNot compile
      }
    }
  }

  "Traversable" should {
    "have conversion to SizedVector" that {
      "succeeds when sizes match" in {
        val result = SizedVector(3, 1, 4)
        Vector(3, 1, 4).toVectorOfSize(3) should equal (result)
        List(3, 1, 4).toVectorOfSize(3) should equal (result)
      }

      "fails when sizes differ" in {
        an [IllegalArgumentException] should be thrownBy {
          Vector(3, 1, 4).toVectorOfSize(2)
        }
        an [IllegalArgumentException] should be thrownBy {
          Vector(3, 1, 4).toVectorOfSize(4)
        }
        an [IllegalArgumentException] should be thrownBy {
          List(3, 1, 4).toVectorOfSize(2)
        }
        an [IllegalArgumentException] should be thrownBy {
          List(3, 1, 4).toVectorOfSize(4)
        }
      }

      "not compiles when provided a runtime size" in {
        val s = 3
        "Vector(3, 1, 4).toVectorOfSize(s)" shouldNot compile
      }
    }
  }
}
object SizedVectorTest {
}
