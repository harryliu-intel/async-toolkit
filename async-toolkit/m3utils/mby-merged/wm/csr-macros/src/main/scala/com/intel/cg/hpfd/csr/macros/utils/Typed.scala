package com.intel.cg.hpfd.csr.macros.utils

import scala.reflect.macros.blackbox.Context
import scala.util.{Try => ErrTry}

/**
  * Toolbox for handling typed trees and expressions.
  */
trait Typed {
  /** Context of the macro */
  val c: Context
  import c.universe._

  /** Integer's operations related to expressions and typed trees. */
  implicit class TypedInt(value: Int) {
    /** Create typename of a integer singleton type, e.g. `5`. */
    def toTypeName(): TypeName = TypeName(value.toString)

    /** Convert to an expression. */
    def toExpr(): Expr[Int] = c.Expr[Int](q"$value")
  }

  /** Expression-related operations. */
  implicit class TypedExpr[T](expr: Expr[T]) {
    /** Evaluate expression encapsulating failure. */
    def tryEval(): ErrTry[T] = ErrTry(c.eval[T](expr))

    /** Evaluate expression or abort compilation. */
    def eval(): T = {
      tryEval.getOrElse(c.abort(expr.tree.pos, s"Expression cannot be evaluated: $expr"))
    }

    /** Evaluate expression to an option. */
    def evalOption(): Option[T] = tryEval.toOption
  }

  /** Integer expression's operations related to expressions and typed trees. */
  implicit class TypedIntExpr(expr: Expr[Int]) {
    /** Create typename of a integer singleton type, e.g. `5`.
      *
      * Only works if expression can be evaluated at compile-time.
      */
    def toTypeName(): TypeName = {
      val pos = expr.tree.pos
      val value = TypedExpr(expr).eval
      value.toTypeName
    }
  }

  /** Typed tree-related operations. */
  implicit class TypedTree(tree: Tree) {
    /** Assign type (unchecked). */
    def toExpr[T](): Expr[T] = c.Expr[T](tree)

    /** Assign type (checked). Exceptions on typecheck error, aborts on type incompatibility. */
    def checkedExpr[T: TypeTag](mode: c.TypecheckMode = c.TERMmode): Expr[T] = {
      val pos = tree.pos
      val tyTree = c.typecheck(tree, mode)  // let the exception flow!
      if (!(tyTree.tpe weak_<:< typeOf[T])) c.abort(pos, "checkedExpr encountered incompatible type!")
      c.Expr[T](tyTree)
    }

    /** Optionally assign type. Nones on both typecheck error or type incompatibility */
    def toExprOption[T: TypeTag](mode: c.TypecheckMode = c.TERMmode): Option[Expr[T]] = {
      ErrTry(c.typecheck(tree, mode))
        .filter(tree => !(tree.tpe weak_<:< typeOf[T]))
        .map(tree => c.Expr[T](tree))
        .toOption
    }
  }
}
