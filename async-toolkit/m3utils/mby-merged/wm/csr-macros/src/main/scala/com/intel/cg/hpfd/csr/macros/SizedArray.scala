package com.intel.cg.hpfd.csr.macros

import scala.reflect.ClassTag
import scala.language.experimental.macros
import scala.reflect.api.Trees
import scala.reflect.macros.whitebox.Context
import shapeless._
import syntax.singleton._
import eu.timepit.refined.W
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._


trait SizedArrayLike[T] extends Traversable[T] {
  def array: Array[T]

  def length: Int
  final def inBounds(idx: Int): Boolean = 0 <= idx && idx < length
  final def checkInBounds(idx: Int): Unit = require(inBounds(idx))

  def applyUnsafe(i: Int): T
  def updateUnsafe(i: Int, value: T): Unit
  def updateUnsafe(i: Int, fn: T => T): Unit = updateUnsafe(i, fn(applyUnsafe(i)))

  def foreach[U](f: T => U): Unit = {
    for(i <- 0 until length) {
      f(applyUnsafe(i))
    }
  }
  protected def loop(f: (Int, T) => T): Unit = {
    for(i <- 0 until length) {
      updateUnsafe(i, f(i, applyUnsafe(i)))
    }
  }
  def update(f: (Int, T) => T): Unit = loop(f)
  def update(f: T => T): Unit = loop((_, el) => f(el))
  def fill(f: => T): Unit = loop((_: Int, _: T) => f)

  override def toString: String = "SizedArray[?](" + map(_.toString).mkString(",") + ")"
  def orgToString: String = array.toString
}

class SizedArray[T: ClassTag, Size](len: Size) extends SizedArrayLike[T] {
  type I = Int Refined Interval.ClosedOpen[W.`0`.T, Size]
  val data = Array.ofDim[T](length)

  lazy val length: Int = len.asInstanceOf[Int]
  def array: Array[T] = data

  def apply(idx: I) = data.apply(idx)
  def applyUnsafe(idx: Int): T = {
    checkInBounds(idx)
    data(idx)
  }

  def update(idx: I, value: T) = data.update(idx, value)
  def updateUnsafe(idx: Int, value: T): Unit = {
    checkInBounds(idx)
    data(idx) = value
  }
  override def updateUnsafe(idx: Int, f: T => T): Unit = {
    checkInBounds(idx)
    data(idx) = f(data(idx))
  }

  protected override def loop(f: (Int, T) => T): Unit = {
    for(i <- 0 until length) {
      data(i) = f(i, data(i))
    }
  }

  def map(f: (Int, T) => T): SizedArray[T, Size] = {
    val res = new SizedArray[T, Size](len)
    res.update((i: Int, _: T) => f(i, data(i)))
    res
  }
  def map(f: T => T): SizedArray[T, Size] = {
    val res = new SizedArray[T, Size](len)
    res.update((i: Int, _: T) => f(data(i)))
    res
  }

  override def toString: String = "SizedArray[" + length + "](" + data.mkString(",") + ")"
}
object SizedArray {
  def apply[T: ClassTag](element: T): SizedArray[T, W.`1`.T] = {
    val result = new SizedArray[T, W.`1`.T](1)
    result(0) = element
    result
  }
  implicit def toScalar[T](array: SizedArray[T, W.`1`.T]): T = array.data(0)

  def ofDim[T](size: Int) = macro ofDimImpl[T]
  def ofDim[T](n1: Int, n2: Int) = macro ofDim2Impl[T]
  def fill[T](size: Int)(el: => T) = macro fillImpl[T]
  def fill[T](n1: Int, n2: Int)(el: => T) = macro fill2Impl[T]
  def apply[T](el0: T, el1: T, rest: T*) = macro applyImpl[T]

  def ofDimImpl[T: c.WeakTypeTag](c: Context)(size: c.Expr[Int]): c.Expr[Any] = {
    import c.universe._
    val sizeVal = c.eval[Int](size)
    val sizeTypeName = TermName(sizeVal.toString)
    val typeName = implicitly[c.WeakTypeTag[T]]
    c.Expr[Any](q"new SizedArray[$typeName, W.`$sizeTypeName`.T]($size)")
  }
  def ofDim2Impl[T: c.WeakTypeTag](c: Context)(n1: c.Expr[Int], n2: c.Expr[Int]): c.Expr[Any] = {
    import c.universe._
    val n1val = c.eval[Int](n1)
    val n2val = c.eval[Int](n2)
    val typeName = implicitly[c.WeakTypeTag[T]]
    c.Expr[Any](q"SizedArray.fill($n1val)(SizedArray.ofDim[$typeName]($n2val))")
  }
  def fillImpl[T: c.WeakTypeTag](c: Context)(size: c.Expr[Int])(el: c.Expr[T]): c.Expr[Any] = {
    import c.universe._
    val sizeVal = c.eval[Int](size)
    val sizeTypeName = TermName(sizeVal.toString)
    val typeName = implicitly[c.WeakTypeTag[T]]

    val decl = q"val result = new SizedArray[$typeName, W.`$sizeTypeName`.T]($size)"
    val body = (0 until sizeVal).map(i => q"result($i) = $el").toList
    val code = decl :: body ::: q"result" :: Nil
    c.Expr[Any](q"..$code")
  }
  def fill2Impl[T: c.WeakTypeTag](c: Context)(n1: c.Expr[Int], n2: c.Expr[Int])(el: c.Expr[T]): c.Expr[Any] = {
    import c.universe._
    val n1val = c.eval[Int](n1)
    val n2val = c.eval[Int](n2)
    c.Expr[Any](q"SizedArray.fill($n1val)(SizedArray.fill($n2val)($el))")
  }
  def applyImpl[T: c.WeakTypeTag](c: Context)(el0: c.Expr[T], el1: c.Expr[T], rest: c.Expr[T]*): c.Expr[Any] = {
    import c.universe._
    val list = el0 :: el1 :: rest.toList
    val size = list.length

    val sizeTypeName = TermName(size.toString)
    val typeName = implicitly[c.WeakTypeTag[T]]
    val decl = q"val result = new SizedArray[$typeName, W.`$sizeTypeName`.T]($size)"
    val body = list.zipWithIndex.map{ case (el,i) => q"result($i) = $el" }
    val code = decl :: body ::: q"result" :: Nil
    c.Expr[Any](q"..$code")
  }
}



/*
var arr10 = new SizedArray[Int, W.`10`.T](10)
    var arr12 = new SizedArray[Int, W.`12`.T](12)

    //arr10 = arr12
    //arr12(-1) = 5
    //arr12(15) = 5
    arr12(11) = 5
    println(s"arr12(11) = ${arr12(11)}")
    println(arr12)
    arr12.update(_ + _)
    println(arr12)
    println(arr12.map(2 * _))
    println(arr12)
    println(arr12.sum)
    println(arr12.splitAt(3))
    println(arr12 ++ arr12)
    ;{
      var i: arr12.I = 0
      i = 0
      println(arr12(i))
      i = 1
      println(arr12(i))
      i = 2
      println(arr12(i))
      val j: W.`3`.T = 3
      println(arr12(j))
    }

    val arr1 = new SizedArray[Long, W.`1`.T](1)
    val scalar1: Long = arr1
    //val scalar12: Long = arr12

    val arr_unknown: SizedArrayLike[Int] = arr12
    //println(arr_unknown.applyUnsafe(12))
    var xx = 12
    //println(arr_unknown.applyUnsafe(xx))
 */