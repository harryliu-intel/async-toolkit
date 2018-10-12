package com.intel.cg.hpfd.csr.macros.annotations

import com.intel.cg.hpfd.madisonbay.BitVector

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros._

@compileTimeOnly("enable macro paradise to expand macro annotations")
class GenOpticsLookup extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro GenOpticsLookupImpl.impl
}

class GenOpticsLookupImpl(val c: whitebox.Context) {
  import c.universe._

  private def generateCodeForList(tpname: c.Name, tname: c.TermName, tpt: c.Tree): c.Tree = {
    val tq"$packageId.$typeId" = tpt
    q"""
      me.$tname
        .zipWithIndex
        .map { case (item, idx) =>
          $packageId.${typeId.toTermName}.genOpticsLookup[A](
            item,
            path composeOptional
              ${tpname.toTermName}.${TermName(s"_$tname")}.asOptional composeOptional
              index(idx)
          )
        }.reduce((left,right) => left.merged(right)(null))
     """
  }

  private def generateCodeForSingleElement(tpname: c.Name, tname: c.TermName, tpt: c.Tree): c.Tree = {
    val tq"$packageId.$typeId" = tpt
    q"""
      $packageId.${typeId.toTermName}.genOpticsLookup[A](
        me.$tname,
        path composeOptional ${tpname.toTermName}.${TermName(s"_$tname")}.asOptional
      )
    """
  }

  private def generateGenOpticsLookup(tpname: c.Name, params: List[ValDef]): c.Tree = {
    val bvecTy = typeOf[BitVector]
    val results: List[c.Tree] = params.map {
      // ignore AddressRange
      case q"$_ val range: AddressRange = $expr" =>
        q"_root_.scala.collection.immutable.HashMap[Address, Optional[A, $bvecTy]]()"
      case q"$_ val $tname: List[$tpt] = $_" => generateCodeForList(tpname, tname, tpt)
      case q"$_ val $tname: $tpt = $_" => generateCodeForSingleElement(tpname, tname, tpt)
      case other => c.abort(
        other.pos,
        "Unknown element. Expected case class' constructor argument here."
      )
    }

    val code = results.reduce((left,right) => q"$left.merged($right)(null)")

    q"""
       def genOpticsLookup[A](
         me: ${tpname.toTypeName},
         path: _root_.monocle.Optional[A,${tpname.toTypeName}]
       ): _root_.scala.collection.immutable.HashMap[Address, _root_.monocle.Optional[A,$bvecTy]] = {
         import _root_.monocle.function.Index._
         import _root_.monocle.Optional

         $code
       }
     """
  }

  def impl(annottees: c.Expr[Any]*): c.Expr[Any] = {
    val input = annottees.map(_.tree).toList

    val output = input match {
      case (classDef @ q"$_ class $tpname[..$_] $_(...$ctorParams) extends { ..$_ } with ..$_ { $_ => ..$_ }")
        :: q"$objMods object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf => ..$objDefs }"
        :: Nil =>
        q"""
         $classDef
         $objMods object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf =>
           ${generateGenOpticsLookup(tpname, ctorParams.head)}
           ..$objDefs
         }
         """
      case (classDef @ q"$_ class ${tpname: c.Name}[..$_] $_(...$ctorParams) extends { ..$_ } with ..$_ { $_ => ..$_ }")
        :: Nil =>
        val name = tpname.toTermName
        q"""
         $classDef
         object $name {
            ${generateGenOpticsLookup(tpname, ctorParams.head)}
         }
         """
      case other => c.abort(wrappingPos(other), "Only case class declarations can be annotated with @GenOpticsLookup")

    }
    c.Expr[Any](output)
  }
}
