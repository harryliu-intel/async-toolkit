package com.intel.cg.hpfd.csr.macros.collections

import com.intel.cg.hpfd.csr.macros.utils.{Hygiene, Typed}
import eu.timepit.refined.W
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.util.Try

import monocle.{Lens, Optional, Getter}
import monocle.function.Index
import scalaz.Applicative

/**
  * Size-aware vector version.
  *
  * Can contain any number of elements a vector can (unlike shapeless' Sized).
  * @param data raw vector
  * @tparam T type of elements
  * @tparam Size size of vector, singleton of {{{Int}}}, should be type-level equivalent of {{{data.length}}}
  */
case class SizedVector[T, Size <: Int] private (data: Vector[T]) extends Traversable[T] {
  /** Refined index type, any integer in bounds {{{0 until length}}}. */
  type I = Int Refined Interval.ClosedOpen[W.`0`.T, Size]

  /** Value-level equivalent of {{{Size}}}. */
  def length: Int = data.length

  /** Value-level equivalent of {{{!(Size =:= W.`0`.T)}}}. */
  override def nonEmpty: Boolean = data.nonEmpty

  /** Getter for data. */
  def vector: Vector[T] = data

  /** Check if a runtime index is a value of the refined index type. */
  def inBounds(idx: Int): Boolean = 0 <= idx && idx < length

  /** Assert a runtime index is in bounds. */
  def checkInBounds(idx: Int): Unit = require(inBounds(idx))

  /** Safely get element at refined index. */
  def apply(idx: I): T = data.apply(idx)

  /** Unsafely get element at runtime index.
    *
    * Aborts if out of bounds.
    */
  def applyUnsafe(idx: Int): T = {
    checkInBounds(idx)
    data(idx)
  }

  /** Tests whether this general sequence contains given index.
    *
    * Alias for [[inBounds]].
    */
  def isDefinedAt(idx: Int): Boolean = inBounds(idx)

  /** Turns this partial function into a plain function returning an {{{Option}}} result.
    *
    * The returned function returns {{{Some}}} if index fulfills refinement and {{{None}}} otherwise.
    */
  def lift: Int => Option[T] = {
    (idx: Int) => if (inBounds(idx)) { Some(data(idx)) } else { None }
  }

  /** Safely updates at a certain refined index. */
  def updated(idx: I, value: T): SizedVector[T, Size] = {
    SizedVector.unsafe[T, Size] {
      data.updated(idx, value)
    }
  }

  /** Safely read-updates at a certain refined index. */
  def updated(idx: I, f: T => T): SizedVector[T, Size] = {
    SizedVector.unsafe[T, Size] {
      data.updated(idx, f(data(idx)))
    }
  }

  /** Unsafely updates at a certain refined index.
    *
    * Aborts if out of bounds.
    */
  def updatedUnsafe(idx: Int, value: T): SizedVector[T, Size] = {
    checkInBounds(idx)
    SizedVector.unsafe[T, Size] {
      data.updated(idx, value)
    }
  }

  /** Safely read-updates at a certain refined index.
    *
    * Aborts if out of bounds.
    */
  def updatedUnsafe(idx: Int, f: T => T): SizedVector[T, Size] = {
    checkInBounds(idx)
    SizedVector.unsafe[T, Size] {
      data.updated(idx, f(data(idx)))
    }
  }

  /** Applies a function to all elements. */
  def foreach[U](f: T => U): Unit = vector.foreach(f)

  /** Builds a new vector by applying a function to all elements */
  def map[S](f: T => S): SizedVector[S, Size] = {
    SizedVector.unsafe[S, Size] {
      data.map(f)
    }
  }

  /** Builds a new vector by applying a function to all elements zipped with indexes. */
  def map[S](f: (T, Int) => S): SizedVector[S, Size] = {
    SizedVector.unsafe[S, Size] {
      data.zipWithIndex.map(f.tupled)
    }
  }

  /** Zips a vector with its indices. */
  def zipWidthIndex(): SizedVector[(T, Int), Size] = {
    SizedVector.unsafe[(T, Int), Size] {
      data.zipWithIndex
    }
  }

  /** Lens-generating refined indexer. */
  def index(idx: I): Lens[SizedVector[T, Size], T] = {
    Lens[SizedVector[T, Size], T] {
      _.apply(idx)
    } {
      value => _.updated(idx, value)
    }
  }

  /** Lens-generating unsafe indexer.
    *
    * Aborts immediately if index not in bounds.
    */
  def indexUnsafe(idx: Int): Lens[SizedVector[T, Size], T] = {
    checkInBounds(idx)
    Lens[SizedVector[T, Size], T] {
      _.applyUnsafe(idx)
    } {
      value => _.updatedUnsafe(idx, value)
    }
  }

  /** Converts a one-element vector to its only element. */
  def toScalar(implicit ev: Size =:= W.`1`.T): T = data(0)

  override def toString: String = "SizedVector[" + length + "](" + data.mkString(",") + ")"
}
object SizedVector {
  //scalastyle:off structural.type
  /** SizedVector of unknown size. */
  type SomeSizVec[T] = SizedVector[T, S] forSome { type S <: Int }

  /** Unsafe constructor from raw vector.
    *
    * Size is not checked.
    */
  def unsafe[T, Size <: Int](vector: Vector[T]): SizedVector[T, Size] = new SizedVector[T, Size](vector)

  /** Produces a sized vector containing the results
    * of some element computation a number of times.
    *
    * The size must be compile-time constant.
    * */
  def fill[T](n: Int)(el: => T): SomeSizVec[T] = macro Impl.fillImpl[T]

  /** Produces a two-dimensional vector containing the results
    * of some element computation a number of times.
    *
    * The dimensions must be compile-time constants.
    */
  def fill[T](n1: Int, n2: Int)(el: => T): SomeSizVec[SomeSizVec[T]] = macro Impl.fill2Impl[T]

  /** Produces a sized vector containing values of a given function
    * over a range of integer values starting from 0.
    *
    * The size must be compile-time constant,
    */
  def tabulate[T](n: Int)(f: Int => T): SomeSizVec[T] = macro Impl.tabulateImpl[T]

  /** Produces a two-dimensional sized vector containing values of a given function
    * over a range of integer values starting from 0.
    *
    * The dimensions must be compile-time constants.
    */
  def tabulate[T](n1: Int, n2: Int)(f: (Int, Int) => T): SomeSizVec[SomeSizVec[T]] = macro Impl.tabulate2Impl[T]

  /** Creates a sized array with the specified elements.
    *
    * The number of arguments has to be a compile-time constant.
    */
  def apply[T](elements: T*): SomeSizVec[T] = macro Impl.applyImpl[T]

  /** Typelevel-compiletime-runtime converter. */
  private class Impl(val c: Context) extends Hygiene with Typed {
    /** Context to be used. */
    import c.universe._

    /** Companion of {{{Vector}}}. */
    val vecComp = typeOf[Vector[_]].companionInst

    /** Companion of {{{SizedVector}}}. */
    val sizVecComp = typeOf[SizedVector[_, _]].companionInst

    /** Compile-time {{{Vector(...)}}}. */
    def vec[T](elements: Expr[T]*): Expr[Vector[T]] = {
      q"$vecComp.apply(..$elements)".toExpr[Vector[T]]
    }

    /** Compile-time {{{SizedVector(...)}}}. */
    def sizVec[T](n: Expr[Int])(vec: Expr[Vector[T]]): Expr[SomeSizVec[T]] = {
      val sizeName = n.toTypeName
      val tree = q"$sizVecComp.unsafe[$sizeName]($vec)"
      tree.toExpr[SomeSizVec[T]]
    }

    /** Compile-time {{{Vector.fill(n)(el)}}}. */
    def vecFill[T](n: Expr[Int])(el: Expr[T]): Expr[Vector[T]] = {
      q"$vecComp.fill($n)($el)".toExpr[Vector[T]]
    }

    /** Compile-time {{{SizedVector.fill(n)(el)}}}. */
    def sizVecFill[T](n: Expr[Int])(el: Expr[T]): Expr[SomeSizVec[T]] = {
      sizVec(n) { vecFill(n)(el) }
    }

    /** Compile-time {{{Vector.tabulate(n)(f)}}}. */
    def vecTab[T](n: Expr[Int])(f: Expr[(Int) => T]): Expr[Vector[T]] = {
      q"$vecComp.tabulate($n)($f)".toExpr[Vector[T]]
    }

    /** Compile-time {{{SizedVector.tabulate(n)(f)}}}. */
    def sizVecTab[T](n: Expr[Int])(f: Expr[(Int) => T]): Expr[SomeSizVec[T]] = {
      sizVec(n) { vecTab(n)(f) }
    }

    /** {{{SizedVector.fill(n)(el)}}} implementation. */
    def fillImpl[T](n: Expr[Int])(el: Expr[T]): Expr[SomeSizVec[T]] = {
      sizVecFill(n)(el)
    }

    /** {{{SizedVector.fill(n1, n2)(el)}}} implementation. */
    def fill2Impl[T](n1: Expr[Int], n2: Expr[Int])(el: Expr[T]): Expr[SomeSizVec[SomeSizVec[T]]] = {
      sizVecFill(n1) {
        sizVecFill(n2)(el)
      }
    }

    /** {{{SizedVector.tabulate(n)(f)}}} implementation. */
    def tabulateImpl[T](n: Expr[Int])(f: Expr[Int => T]): Expr[SomeSizVec[T]] = {
      sizVecTab(n)(f)
    }

    /** {{{SizedVector.tabulate(n1, n2)(f)}}} implementation. */
    def tabulate2Impl[T](n1: Expr[Int], n2: Expr[Int])(f: Expr[(Int, Int) => T]): Expr[SomeSizVec[SomeSizVec[T]]] = {
      sizVec(n1) {
        vecTab(n1) {
          q"""{ i => ${
            sizVecTab(n2) {
                q"{ j => $f(i, j) }".toExpr[Int => T]
            }
          }}""".toExpr[Int => SomeSizVec[T]]
        }
      }
    }

    /** {{{SizedVector(...)}}} implementation. */
    def applyImpl[T](elements: Expr[T]*): Expr[SomeSizVec[T]] = {
      val n = elements.length.toExpr
      sizVec(n) {
        vec(elements: _*)
      }
    }
  }

  /** Monocle's Getter for {{{SizedVector}}}. */
  implicit def sizVecGetVec[T, Size <: Int]: Getter[SizedVector[T, Size], Vector[T]] = Getter(_.vector)

  /** Monocle's Index for {{{SizedVector}}}. */
  implicit def sizVecIndex[T, Size <: Int]: Index[SizedVector[T, Size], Int, T] = {
    new Index[SizedVector[T, Size], Int, T] {  // handy Index constructor is available in other versions of the lib
      override def index(i: Int): Optional[SizedVector[T, Size], T] =
        Optional[SizedVector[T, Size], T] {
          _.lift(i)
        } {
          el => v => Try(v.updatedUnsafe(i, el)).getOrElse(v)
        }
    }
  }
}
