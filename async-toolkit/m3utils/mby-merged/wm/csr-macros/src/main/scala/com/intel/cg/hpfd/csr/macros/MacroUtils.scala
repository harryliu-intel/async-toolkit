package com.intel.cg.hpfd.csr.macros

import scala.reflect.ClassTag
import scala.reflect.macros.blackbox
import scala.reflect.macros.blackbox.Context
import scala.util.{Try => ErrTry}

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
      //scalastyle:off method.name
      def |(str: => String)(implicit pos: Position): Unit = cAssert(pos, cond, str)
    }

    def cGet[T](pos: Position, option: Option[T], str: String): T = option.getOrElse(abort(pos, str))
    implicit class cGettable[T](op: Option[T]) {
      def cGet(str: String)(implicit pos: Position): T = self.cGet(pos, op, str)
      // scalastyle:off method.name
      def |(str: String)(implicit pos: Position): T = self.cGet(pos, op, str)
    }
  }


  trait HygienicUnquote {
    val c: Context
    import c.universe._


    def appliedTypeTree(tree: c.Tree, args: c.Tree*): c.Tree = tq"$tree[..$args]"
    def appliedTypeTree(tree: c.Tree, args: List[c.Tree]): c.Tree = appliedTypeTree(tree, args: _*)
    def appliedTypeTree(sym: Symbol, args: c.Tree*): c.Tree = appliedTypeTree(q"$sym", args: _*)
    def appliedTypeTree(sym: Symbol, args: List[c.Tree]): c.Tree = appliedTypeTree(sym, args: _*)
    def appliedTypeTree(ty: Type, args: c.Tree*): c.Tree = appliedTypeTree(ty.typeSymbol, args: _*)
    def appliedTypeTree(ty: Type, args: List[c.Tree]): c.Tree = appliedTypeTree(ty.typeSymbol, args: _*)

    implicit class HygienicType(ty: Type) {
      def companionInst: c.Tree = q"${ty.typeSymbol.companion}"
    }

    implicit class HygienicTree(tree: c.Tree) {
      def checkType(ty: Type, mode: c.TypecheckMode = c.TERMmode): Boolean =
        ErrTry(c.typecheck(tree, mode).tpe =:= ty).getOrElse(false)

      def namesInstanceOf[T: TypeTag]: Boolean = checkType(typeOf[T])
      def namesCompanionOf[T: TypeTag]: Boolean = checkType(typeOf[T].companion)
      def isRawNameOf[T: ClassTag]: Boolean = tree.toString == implicitly[ClassTag[T]].runtimeClass.getSimpleName
      def isNameOf[T: TypeTag]: Boolean = checkType(typeOf[T], c.TYPEmode)
    }
    implicit class HygienicTermName(name: c.TermName) {
      def namesInstanceOf[T: TypeTag]: Boolean = q"$name".namesInstanceOf[T]
      def namesCompanionOf[T: TypeTag]: Boolean = q"$name".namesCompanionOf[T]
    }
    implicit class HygienicTypeName(name: c.TypeName) {
      def isRawNameOf[T: ClassTag]: Boolean = tq"$name".isRawNameOf[T]
      def isNameOf[T: TypeTag]: Boolean = tq"$name".namesInstanceOf[T]
    }

    case class InstanceOf[T: TypeTag](tree: c.Tree) {}
    implicit def instanceOfUnlift[T: TypeTag]: Unliftable[InstanceOf[T]] = Unliftable[InstanceOf[T]] {
      case tree @ Ident(TermName(_)) if tree.namesInstanceOf[T] => InstanceOf[T](tree)
      case tree @ Select(_, _) if tree.namesInstanceOf[T] => InstanceOf[T](tree)
    }
    case class CompanionOf[T: TypeTag](tree: c.Tree) {}
    implicit def companionOfUnlift[T: TypeTag]: Unliftable[CompanionOf[T]] = Unliftable[CompanionOf[T]] {
      case tree @ Ident(TermName(_)) if tree.namesCompanionOf[T] => CompanionOf[T](tree)
      case tree @ Select(_, _) if tree.namesCompanionOf[T] => CompanionOf[T](tree)
    }
    case class AliasOf[T: TypeTag](tree: c.Tree) {}
    implicit def aliasOfUnlift[T: TypeTag]: Unliftable[AliasOf[T]] = Unliftable[AliasOf[T]] {
      case tree @ Ident(TypeName(_)) if tree.isNameOf[T] => AliasOf[T](tree)
      case tree @ Select(_, _) if tree.isNameOf[T] => AliasOf[T](tree)
    }
    case class RawNameOf[T: ClassTag](name: TypeName) {}
    implicit def rawNameOfUnlift[T: ClassTag]: Unliftable[RawNameOf[T]] = Unliftable[RawNameOf[T]] {
      case Ident(name @ TypeName(_)) if name.isRawNameOf[T] => RawNameOf[T](name)
    }
    case class RawNameOfCompanion[T: ClassTag](name: TermName) {}
    implicit def rawNameOfCompanionUnlift[T: ClassTag]: Unliftable[RawNameOfCompanion[T]] = Unliftable[RawNameOfCompanion[T]] {
      case Ident(name @ TermName(_)) if q"$name".isRawNameOf[T] => RawNameOfCompanion[T](name)
    }
  }
}
