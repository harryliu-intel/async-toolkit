package com.intel.cg.hpfd.csr.macros.utils

import scala.reflect.macros.blackbox


/**
  * Custom control structures for handy communicates and error handling in macros.
  *
  * Many of utilities provided use implicit position. The idea is to provide an implicit
  * every time some new subtree is handled, e.g. before pattern matching on it.
  */
trait Control { self =>
  /** Context of the macro. */
  val c: blackbox.Context  // whitebox's one is blackbox's one's subtype
  import c._


  /** Aborts compilation unconditionally.
    *
    * Macro fails instantly, no returning.
    */
  def cAbort(pos: Position, str: String): Nothing = abort(pos, str)

  /** Aborts compilation unconditionally.
    *
    * Macro fails instantly, no returning.
    */
  def cAbort(str: String)(implicit pos: Position): Nothing = cAbort(pos, str)

  /** Emits compilation error.
    *
    * Macro may continue to do other operations after that, potentially emitting more errors.
    */
  def cError(pos: Position, str: String): Unit = error(pos, str)

  /** Emits compilation error.
    *
    * Macro may continue to do other operations after that, potentially emitting more errors.
    */
  def cError(str: String)(implicit pos: Position): Unit = cError(pos, str)

  /** Emits information at compile time. Macro goes on.
    *
    * Supressed unless `-verbose` of `flag=true`.
    */
  def cInfo(pos: Position, str: String, flag: Boolean = false): Unit = info(pos, str, flag)

  /** Emits information at compile time. Macro goes on.
    *
    * Supressed unless `-verbose` of `flag=true`.
    */
  def cInfo(str: String, flag: Boolean)(implicit pos: Position): Unit = cInfo(pos, str, flag)

  /** Emits information at compile time. Macro goes on.
    *
    * Supressed unless `-verbose` of `flag=true`.
    */
  def cInfo(str: String)(implicit pos: Position): Unit = cInfo(pos, str)

  /** Emits warning at compile time. Macro goes on. */
  def cWarn(pos: Position, str: String): Unit = warning(pos, str)

  /** Emits warning at compile time. Macro goes on. */
  def cWarn(str: String)(implicit pos: Position): Unit = cWarn(pos, str)

  /** Asserts certain condition. If it fails, aborts instantly. */
  def cAssert(pos: Position, cond: Boolean, str: => String): Unit = if (!cond) { cAbort(pos, str) }

  /** Asserts certain condition. If it fails, aborts instantly. */
  def cAssert(cond: Boolean)(str: => String)(implicit pos: Position): Unit = cAssert(pos, cond, str)

  /** Helper for graphical form of assertions.
    *
    * Boolean expression can be graphically (via `|`) separated from message string
    * to be shown when that condition is false.
    *
    * Example:
    * {{{
    *   (stack.size > 0)  |  "stack is empty!"
    *   val el = stack.pop()
    *   el.isValid        |  "element isn't valid!"
    * }}}
    */
  implicit class cAssertable(cond: Boolean) {
    //scalastyle:off method.name
    /** Graphical separator in asserts. */
    def |(str: => String)(implicit pos: Position): Unit = cAssert(pos, cond, str)
  }

  /** Unwrapping assertion for options. */
  def cGet[T](pos: Position, option: Option[T], str: String): T = option.getOrElse(abort(pos, str))

  /** Helper option unwraping assertions.
    *
    * Options can be separated from message string to be show when option is `None`.
    * It can be done either graphically (via `|`) or as a method call (via `.cGet`)
    *
    * Example:
    * {{{
    *   val el = stack.head_option  |  "stack is empty!"
    *   val info = el.info          |  "no info for element!"
    * }}}
    */
  implicit class cGettable[T](op: Option[T]) {
    /** Method-style assertion. */
    def cGet(str: String)(implicit pos: Position): T = self.cGet(pos, op, str)

    // scalastyle:off method.name
    /** Operator-style assertion. */
    def |(str: String)(implicit pos: Position): T = self.cGet(pos, op, str)
  }
}
