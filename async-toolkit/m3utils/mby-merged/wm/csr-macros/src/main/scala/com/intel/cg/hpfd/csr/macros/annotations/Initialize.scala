package com.intel.cg.hpfd.csr.macros.annotations

import com.intel.cg.hpfd.csr.macros.utils.{Control, Hygiene}
import com.intel.cg.hpfd.madisonbay.Memory.AddressRange

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros._

import monocle.Optional

class OfSize(val length: Int) extends StaticAnnotation
class At(val address: Long) extends StaticAnnotation
class Modulo(val address: Long) extends StaticAnnotation
class Increment(val address: Long) extends StaticAnnotation

case class RdlAnnotations(ofSize: Option[OfSize], at: Option[At], modulo: Option[Modulo], increment: Increment = new Increment(0L))

trait LiftableAnnotations {
  val c: blackbox.Context
  import c.universe._

  implicit val unliftAt = Unliftable[At] {
    case q"new At(${address: Long})"  => new At(address)
    case q"new At(${address: Int})" => new At(address)
  }
  implicit val unliftOfSize = Unliftable[OfSize] {
    case q"new OfSize(${size: Int})"  => new OfSize(size)
    case q"new OfSize(${size: Long})" => new OfSize(size.toInt)
  }
  implicit val unliftModulo = Unliftable[Modulo] {
    case q"new Modulo(${address: Int})"  => new Modulo(address)
    case q"new Modulo(${address: Long})" => new Modulo(address)
  }
  implicit val unliftIncrement = Unliftable[Increment] {
    case q"new Increment(${address: Int})"  => new Increment(address)
    case q"new Increment(${address: Long})" => new Increment(address)
  }
}

@compileTimeOnly("enable macro paradise to expand macro annotations")
class Initialize extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro InitializeImpl.impl
}

class InitializeImpl(val c: whitebox.Context) extends LiftableAnnotations with Control with Hygiene {
  import c.universe._

  val nextAddressSuffix = "NextFreeAddress"

  case class ArgumentContext(nextAddressName: TermName, argumentName: TermName, quoteFun: (TermName, Tree) => Tree)

  /**
    * Takes address value of either
    *  '%=' (aka mod alignment)
    *  or
    *  '@' (aka absolute to local scope)
    * from rdl definition
    */
  private def obtainAddress(annotations: RdlAnnotations, prevAddress: TermName): Tree = {
    import annotations._
    at.map(a => q"address + ${a.address}.bytes")
      .orElse(modulo.map(mod => q"$prevAddress.alignTo(Alignment(${mod.address}.bytes))"))
      .getOrElse(q"$prevAddress")
  }

  private def generateCodeForList(tname: TermName, tpt: Tree, annotations: RdlAnnotations): ArgumentContext = {
    import annotations._
    val nextAddressName = TermName(s"$tname$nextAddressSuffix")
    val (termId, typeId) = tpt.toTermTypePair
    // TODO: change me!
    val listLength: Int = ofSize.map(_.length).getOrElse(1) // we are already in the context where 'OfSize' is present
    val inc = increment.address

    lazy val nextFreeAddress: Tree = if (inc == 0) q"child.range.lim" else q"child.range.pos + $inc.bytes"

    def quoteFun(prevAddress: TermName, continuation: Tree): Tree =
      q"""
        val initialAddress: Address = ${obtainAddress(annotations, prevAddress)}
        val (_, result @ lastChild :: _) = (1 to $listLength).foldLeft((initialAddress, List.empty[$typeId])) {
           case ((nextAddress, res), _) =>
             val child = $termId.apply(nextAddress)
             ($nextFreeAddress, child :: res)
        }
        val ($nextAddressName, $tname) = (lastChild.range.lim, result.reverse)
        $continuation
      """

    ArgumentContext(nextAddressName = nextAddressName, argumentName = tname.toTermName, quoteFun = quoteFun)
  }

  private def generateCodeForSingleElement(tname: TermName, tpt: Tree, annotations: RdlAnnotations): ArgumentContext = {
    val nextAddress = TermName(s"$tname$nextAddressSuffix")

    val (termId, typeId) = tpt.toTermTypePair

    /**
      * In case of '@' rdl modifier, we have the current scope address - it is simply referenced
      * by its name - 'address'
      */
    def quoteFun(prevAddress: TermName, continuation: Tree) =
      q"""
        val ($nextAddress, $tname) = {
          val child = $termId(
            ${obtainAddress(annotations, prevAddress)}
          )
          (child.range.lim, child)
        }
        $continuation
      """

    ArgumentContext(nextAddressName = nextAddress, argumentName = tname.toTermName, quoteFun = quoteFun)
  }

  /**
    * Pattern match over all possible modifiers like @At, @Modulo, @Increment, @OfSize
    * TODO: Add Rdl specific validation like "@ and %= are mutually exclusive
    */
  private def toRdlAnnotations(lt: List[Tree]): RdlAnnotations =
    lt.foldLeft(RdlAnnotations(None, None, None)) {
      /*op: (Tree, mods) => B)*/
      case (a @ RdlAnnotations(None, _, _, _), q"${ofSize: OfSize}") => a.copy(ofSize = Some(ofSize))
      case (a @ RdlAnnotations(_, None, _, _), q"${at: At}") => a.copy(at = Some(at))
      case (a @ RdlAnnotations(_, _, None, _), q"${modulo: Modulo}") => a.copy(modulo = Some(modulo))
      case (a, q"${inc: Increment}") => a.copy(increment = inc)
      case (_, other) => cAbort(other.pos, "Found unexpected annotation")
    }

  // TODO: get rid of (Address | AddressRange) hardcodes, maybe more validation
  def generateApplyMethod(tpname: TypeName, params: List[ValDef]): Tree = {
    implicit val pos = wrappingPos(params)
    val otherParams = params match {
      case q"$_ val range: ${_: AliasOf[AddressRange]} = $_" :: tail => tail
      case _ => cAbort("macro @Initialize expects case class' constructor to be of shape: (range: AddressRange, ...)")
    }

    val results = otherParams.map {
      case q"$mods val $tname: $tpt = $_" =>
        val annots = toRdlAnnotations(mods.annotations)
        tpt match {
          case tq"$_[$tpt]" => generateCodeForList(tname, tpt, annots)
          case _ => generateCodeForSingleElement(tname, tpt, annots)
        }
      case other =>
        cAbort(other.pos, "Unknown element. Expected case class' constructor argument here.")
    }

    val lastAddressName = results.last.nextAddressName
    val constructorArgs = q"AddressRange(address, $lastAddressName)" :: results.map(r => q"${r.argumentName}")
    val creationExpression = q"${tpname.toTermName}(..$constructorArgs)"

    val resultRecipt: TermName => Tree =
      results.foldRight((_: TermName) => creationExpression) {
        case (ArgumentContext(nextAddrName, _, quoteFun), continuation) =>
          termName => quoteFun(termName, continuation(nextAddrName))
      }

    q"""
       def apply(address: Address): ${tpname.toTypeName} = {
         ..${resultRecipt(TermName("address"))}
       }
     """
  }

  def impl(annottees: Expr[Any]*): Expr[Any] = {
    val input = annottees.map(_.tree).toList

    implicit val pos = wrappingPos(input)
    val output = input match {
      case q"${cls: ClassDef}" :: _ if !cls.mods.hasFlag(Flag.CASE) =>
        cAbort("only case classes allowed in @Initialize")

      // Sadly, ClassDef only knows its type parameters, not constructor parameters;
      // they are available via `def <init>` on it's template body list.
      case q"$_ class $_[..$_] $_(...$ctorParams) extends { ..$_ } with ..$_ { $_ => ..$_ }" :: _
        if ctorParams.length > 1 => cAbort("only single parameter list classes allowed in @Initialize")

      case (classDef @ q"$_ class $tpname[..$_] $_(..$ctorParams) extends { ..$_ } with ..$_ { $_ => ..$_ }")
        :: q"$objMods object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf => ..$objDefs }"
        :: Nil =>
        q"""
         $classDef
         $objMods object $objName extends { ..$objEarlyDefs} with ..$objParents { $objSelf =>
           ${generateApplyMethod(tpname, ctorParams)}
           ..$objDefs
         }
         """
      case (classDef @ q"$_ class $tpname[..$_] $_(..$ctorParams) extends { ..$_ } with ..$_ { $_ => ..$_ }")
        :: Nil =>
        val objName = tpname.toTermName
        q"""
         $classDef
         object $objName {
           ${generateApplyMethod(tpname, ctorParams)}
         }
         """
      case _ => cAbort("only case class declarations can be annotated with @Initialize")
    }
    c.Expr[Any](output)
  }
}
