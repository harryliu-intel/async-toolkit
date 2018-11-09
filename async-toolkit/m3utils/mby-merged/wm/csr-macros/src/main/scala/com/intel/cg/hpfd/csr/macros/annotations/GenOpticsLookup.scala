package com.intel.cg.hpfd.csr.macros.annotations

import com.intel.cg.hpfd.csr.macros.utils.{Control, Hygiene}
import com.intel.cg.hpfd.madisonbay.BitVector
import com.intel.cg.hpfd.madisonbay.Memory.{Address, AddressRange}

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros._

import monocle.Optional

@compileTimeOnly("enable macro paradise to expand macro annotations")
class GenOpticsLookup extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro GenOpticsLookupImpl.impl
}

class GenOpticsLookupImpl(val c: whitebox.Context) extends Control with Hygiene {
  import c.universe._

  private def generateCodeForList(tpname: TypeName, tname: TermName, tpt: Tree): Tree = {
    val (termId, _) = tpt.toTermTypePair
    q"""
      me.$tname
        .zipWithIndex
        .map { case (item, idx) =>
          $termId.genOpticsLookup[A](
            item,
            path composeOptional
              ${tpname.toTermName}.${TermName(s"_$tname")}.asOptional composeOptional
              index(idx)
          )
        }.reduce((left,right) => left.merged(right)(null))
     """
  }

  private def generateCodeForSingleElement(tpname: TypeName, tname: TermName, tpt: Tree): Tree = {
    val (termId, _) = tpt.toTermTypePair
    q"""
      $termId.genOpticsLookup[A](
        me.$tname,
        path composeOptional ${tpname.toTermName}.${TermName(s"_$tname")}.asOptional
      )
    """
  }

  private def generateGenOpticsLookup(tpname: TypeName, params: List[ValDef]): Tree = {
    import scala.collection.immutable.HashMap

    val bvecTy = typeOf[BitVector]
    val addressTy = typeOf[Address]
    val opTpTy = appliedTypeTree(symbolOf[Optional[_, _]], tq"A", tq"$tpname")
    val opBvecTy = appliedTypeTree(symbolOf[Optional[_, _]], tq"A", tq"$bvecTy")
    val hashMapTy = appliedTypeTree(typeOf[HashMap[_, _]], tq"$addressTy", opBvecTy)
    val hashMapComp = symbolOf[HashMap[_, _]].companion
    val hashMapConstr = q"$hashMapComp.apply[$addressTy, $opBvecTy]()"

    val results: List[Tree] = params.map {
      // ignore AddressRange
      case q"$_ val range: ${_: AliasOf[AddressRange]} = $expr" => hashMapConstr
      case q"$_ val $tname: $_[$tpt] = $_" => generateCodeForList(tpname, tname, tpt)
      case q"$_ val $tname: $tpt = $_" => generateCodeForSingleElement(tpname, tname, tpt)
      case other => cAbort(other.pos, "Unknown element. Expected case class' constructor argument here.")
    }

    val code = results.reduce((left,right) => q"$left.merged($right)(null)")

    q"""
       def genOpticsLookup[A](
         me: $tpname,
         path: $opTpTy
       ): $hashMapTy = {
         import _root_.monocle.function.Index._
         import _root_.monocle.Optional

         $code
       }
     """
  }

  def impl(annottees: Expr[Any]*): Expr[Any] = {
    val input = annottees.map(_.tree).toList

    implicit val pos = wrappingPos(input)
    val output = input match {
      case q"${cls: ClassDef}" :: _ if !cls.mods.hasFlag(Flag.CASE) =>
        cAbort("only case classes allowed in @GenOpticsLookup")

      // Sadly, ClassDef only knows its type parameters, not constructor parameters;
      // they are available via `def <init>` on it's template body list.
      case q"$_ class $_[..$_] $_(...$ctorParams) extends { ..$_ } with ..$_ { $_ => ..$_ }" :: _
        if ctorParams.length > 1 => cAbort("only single parameter list classes allowed in @GenOpticsLookup")

      case (classDef @ q"$_ class $tpname[..$_] $_(..$ctorParams) extends { ..$_ } with ..$_ { $_ => ..$_ }")
        :: q"$objMods object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf => ..$objDefs }"
        :: Nil =>
        q"""
         $classDef
         $objMods object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf =>
           ${generateGenOpticsLookup(tpname, ctorParams)}
           ..$objDefs
         }
         """
      case (classDef @ q"$_ class $tpname[..$_] $_(..$ctorParams) extends { ..$_ } with ..$_ { $_ => ..$_ }")
        :: Nil =>
        val name = tpname.toTermName
        q"""
         $classDef
         object $name {
            ${generateGenOpticsLookup(tpname, ctorParams)}
         }
         """
      case _ => cAbort("Only case class declarations can be annotated with @GenOpticsLookup")
    }
    c.Expr[Any](output)
  }
}
