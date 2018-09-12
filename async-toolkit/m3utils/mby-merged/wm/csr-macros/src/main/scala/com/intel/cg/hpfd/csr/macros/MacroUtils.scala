package com.intel.cg.hpfd.csr.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox


object MacroUtils {
  trait Control { self =>
    val c: blackbox.Context  // whitebox's one is blackbox's one's subtype
    import c._

    def cAbort(pos: Position, str: String): Nothing = abort(pos, str)
    def cAbort(str: String)(implicit pos: Position): Nothing = cAbort(pos, str)

    def cError(pos: Position, str: String): Unit = error(pos, str)
    def cError(str: String)(implicit pos: Position): Unit = cError(pos, str)

    def cInfo(pos: Position, str: String, flag: Boolean = false): Unit = info(pos, str, flag)
    def cInfo(str: String, flag: Boolean)(implicit pos: Position): Unit = cInfo(pos, str, flag)
    def cInfo(str: String)(implicit pos: Position): Unit = cInfo(pos, str)

    def cWarn(pos: Position, str: String): Unit = warning(pos, str)
    def cWarn(str: String)(implicit pos: Position): Unit = cWarn(pos, str)

    def cAssert(pos: Position, cond: Boolean, str: => String): Unit = if (!cond) { cAbort(pos, str) }
    def cAssert(cond: Boolean)(str: => String)(implicit pos: Position): Unit = cAssert(pos, cond, str)
    implicit class cAssertable(cond: Boolean) {
      def |(str: => String)(implicit pos: Position): Unit = cAssert(pos, cond, str)
    }

    def cGet[T](pos: Position, option: Option[T], str: String): T = option.getOrElse(abort(pos, str))
    implicit class cGettable[T](op: Option[T]) {
      def cGet(str: String)(implicit pos: Position): T = self.cGet(pos, op, str)
      def |(str: String)(implicit pos: Position): T = self.cGet(pos, op, str)
    }
  }
}