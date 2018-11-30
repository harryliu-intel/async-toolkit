package com.intel.cg.hpfd.csr.macros.utils

import scala.reflect.ClassTag
import scala.reflect.macros.blackbox.Context
import scala.util.{Try => ErrTry}

/**
  * Toolbox for macro hygiene-related utilities.
  */
trait Hygiene {
  /** Context of the macro */
  val c: Context
  import c.universe._

  /** Variant of [[appliedType]] for manually hygienic trees.
    *
    * Makes it possible to reference types just being declared or declared in the same scope.
    */
  def appliedTypeTree(tree: Tree, args: Tree*): Tree = tq"$tree[..$args]"

  /** Variant of [[appliedType]] for manually hygienic trees.
    *
    * Makes it possible to reference types just being declared or declared in the same scope.
    */
  def appliedTypeTree(tree: Tree, args: List[Tree]): Tree = appliedTypeTree(tree, args: _*)

  /** Variant of [[appliedType]] for symbol as the higher-order type and hygienic trees as arguments.
    *
    * Makes it possible to reference types just being declared or declared in the same scope.
    */
  def appliedTypeTree(sym: Symbol, args: Tree*): Tree = appliedTypeTree(tq"$sym", args: _*)

  /** Variant of [[appliedType]] for symbol as the higher-order type and hygienic trees as arguments.
    *
    * Makes it possible to reference types just being declared or declared in the same scope.
    */
  def appliedTypeTree(sym: Symbol, args: List[Tree]): Tree = appliedTypeTree(sym, args: _*)

  /** Variant of [[appliedType]] for already declared higher-order type and hygienic trees as arguments.
    *
    * Makes it possible to reference types just being declared or declared in the same scope.
    */
  def appliedTypeTree(ty: Type, args: Tree*): Tree = appliedTypeTree(ty.typeSymbol, args: _*)

  /** Variant of [[appliedType]] for already declared higher-order type and hygienic trees as arguments.
    *
    * Makes it possible to reference types just being declared or declared in the same scope.
    */
  def appliedTypeTree(ty: Type, args: List[Tree]): Tree = appliedTypeTree(ty.typeSymbol, args: _*)

  /** Helper providing hygienic operation on types. */
  implicit class HygienicType(ty: Type) {
    /** Generates hygienic tree representing companion. */
    def companionInst: Tree = q"${ty.typeSymbol.companion}"
  }

  /** Gets stringified name of the class. */
  def rawName[T: ClassTag]: String = implicitly[ClassTag[T]].runtimeClass.getSimpleName

  /** Helper providing hygienic operation on trees. */
  implicit class HygienicTree(tree: Tree) {
    /** Checks tree's return type.
      *
      * If typecheck mode is wrong, it simply returns false (!).
      */
    def checkType(ty: Type, mode: c.TypecheckMode = c.TERMmode): Boolean =
      ErrTry(c.typecheck(tree, mode).tpe =:= ty).getOrElse(false)

    /** Checks whenever tree returns an instance of type. */
    def namesInstanceOf[T: TypeTag]: Boolean = checkType(typeOf[T])

    /** Checks whenever tree returns type's companion. */
    def namesCompanionOf[T: TypeTag]: Boolean = checkType(typeOf[T].companion)

    /** Checks whenever tree is raw identifier of the type. */
    def isRawNameOf[T: ClassTag]: Boolean = tree match {
      case Ident(TypeName(str)) => (str == rawName[T])
      case _ => false
    }

    /** Checks whenever tree is raw identifier of the type's companion. */
    def isRawCompanionNameOf[T: ClassTag]: Boolean = tree match {
      case Ident(TermName(str)) => (str == rawName[T])
      case _ => false
    }

    /** Checks whenever tree returns the type or its alias. */
    def isNameOf[T: TypeTag]: Boolean = checkType(typeOf[T], c.TYPEmode)
  }

  /** Helper providing hygienic operation on TermNames. */
  implicit class HygienicTermName(name: TermName) {
    /** Checks whenever name names an instance of type. */
    def namesInstanceOf[T: TypeTag]: Boolean = q"$name".namesInstanceOf[T]

    /** Checks whenever name names type's companion. */
    def namesCompanionOf[T: TypeTag]: Boolean = q"$name".namesCompanionOf[T]

    /** Checks whenever name is the raw name of type's companion. */
    def isRawCompanionNameOf[T: ClassTag]: Boolean = q"$name".isRawCompanionNameOf[T]
  }

  /** Helper providing hygienic operation on TypeNames. */
  implicit class HygienicTypeName(name: TypeName) {
    /** Checks whenever name is raw name of the type. */
    def isRawNameOf[T: ClassTag]: Boolean = tq"$name".isRawNameOf[T]

    /** Checks whenever name is type's name or its alias. */
    def isNameOf[T: TypeTag]: Boolean = tq"$name".namesInstanceOf[T]
  }

  /** Wrapper to unlift trees returning instances of type. */
  case class InstanceOf[T: TypeTag](tree: Tree) {}

  /** Unlifts trees representing instances of type. */
  implicit def instanceOfUnlift[T: TypeTag]: Unliftable[InstanceOf[T]] = Unliftable[InstanceOf[T]] {
    case tree @ Ident(TermName(_)) if tree.namesInstanceOf[T] => InstanceOf[T](tree)
    case tree @ Select(_, _) if tree.namesInstanceOf[T] => InstanceOf[T](tree)
  }

  /** Wrapper to unlift trees returning companion of type. */
  case class CompanionOf[T: TypeTag](tree: Tree) {}

  /** Unlifts trees representing companion of type. */
  implicit def companionOfUnlift[T: TypeTag]: Unliftable[CompanionOf[T]] = Unliftable[CompanionOf[T]] {
    case tree @ Ident(TermName(_)) if tree.namesCompanionOf[T] => CompanionOf[T](tree)
    case tree @ Select(_, _) if tree.namesCompanionOf[T] => CompanionOf[T](tree)
  }

  /** Wrapper for trees representing type aliases. */
  case class AliasOf[T: TypeTag](tree: Tree) {}

  /** Unlifts trees representing type aliases. */
  implicit def aliasOfUnlift[T: TypeTag]: Unliftable[AliasOf[T]] = Unliftable[AliasOf[T]] {
    case tree @ Ident(TypeName(_)) if tree.isNameOf[T] => AliasOf[T](tree)
    case tree @ Select(_, _) if tree.isNameOf[T] => AliasOf[T](tree)
  }

  /** Wrapper for trees representing raw name of type. */
  case class RawNameOf[T: ClassTag](name: TypeName) {}

  /** Unlifts trees representing raw name of type. */
  implicit def rawNameOfUnlift[T: ClassTag]: Unliftable[RawNameOf[T]] = Unliftable[RawNameOf[T]] {
    case Ident(name @ TypeName(_)) if name.isRawNameOf[T] => RawNameOf[T](name)
  }

  /** Wrapper for trees representing raw name of type's companion. */
  case class RawNameOfCompanion[T: ClassTag](name: TermName) {}

  /** Unlifts trees representing raw name of type's companion. */
  implicit def rawNameOfCompanionUnlift[T: ClassTag]: Unliftable[RawNameOfCompanion[T]] = Unliftable[RawNameOfCompanion[T]] {
    case Ident(name @ TermName(_)) if q"$name".isRawCompanionNameOf[T] => RawNameOfCompanion[T](name)
  }


  /** Convertible to typepath-termpath pair. */
  implicit class TermTypePairable(tree: Tree) {
    /** Converts an identifier or path to a pair of its term-type variants.
      *
      * Typically used to provide companion path from type or vice versa.
      */
    def toTermTypePair(): (Tree, Tree) = {
      tree match {
        case Ident(name) => (Ident(name.toTermName), Ident(name.toTypeName))
        case Select(path, name) => (Select(path, name.toTermName), Select(path, name.toTypeName))
        case _ => c.abort(tree.pos, "toTypeTermPair fed with non-path tree: " + showRaw(tree))
      }
    }
  }
}
