package com.intel.cg.hpfd.csr.macros.annotations

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros._

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

class InitializeImpl(val c: whitebox.Context) extends LiftableAnnotations {
  import c.universe._

  val nextAddressSuffix = "NextFreeAddress"

  case class ArgumentContext(nextAddressName: TermName, argumentName: TermName, quoteFun: (TermName, c.Tree) => c.Tree)

  /**
    * Takes address value of either
    *  '%=' (aka mod alignment)
    *  or
    *  '@' (aka absolute to local scope)
    * from rdl definition
    */
  private def obtainAddress(annotations: RdlAnnotations, prevAddress: TermName): c.Tree = {
    import annotations._
    at.map(a => q"address + ${a.address}.bytes")
      .orElse(modulo.map(mod => q"${prevAddress}.alignTo(Alignment(${mod.address}.bytes))"))
      .getOrElse(q"$prevAddress")
  }

  private def generateCodeForList(tname: c.TermName, tpt: c.Tree, annotations: RdlAnnotations): ArgumentContext = {
    import annotations._
    val nextAddressName = TermName(s"$tname$nextAddressSuffix")
    val AppliedTypeTree(Ident(TypeName(_)), List(Select(Ident(packageId), typeId))) = tpt
    // TODO: change me!
    val listLength: Int = ofSize.map(_.length).getOrElse(1) // we are already in the context where 'OfSize' is present
    val inc = increment.address

    lazy val nextFreeAddress: c.Tree = if (inc == 0) q"child.range.lim" else q"child.range.pos + ${inc}.bytes"

    def quoteFun(prevAddress: TermName, continuation: c.Tree): c.Tree =
      q"""
        val initialAddress: Address = ${obtainAddress(annotations, prevAddress)}
        val (_, result @ lastChild :: _) = (1 to ${listLength}).foldLeft((initialAddress, List.empty[${packageId.toTermName}.${typeId.toTypeName}])) {
           case ((nextAddress, res), _) =>
             val child = ${packageId.toTermName}.${typeId.toTermName}.apply(nextAddress)
             ($nextFreeAddress, child :: res)
        }
        val ($nextAddressName, ${tname.toTermName}) = (lastChild.range.lim, result.reverse)
        $continuation
      """

    ArgumentContext(nextAddressName = nextAddressName, argumentName = tname.toTermName, quoteFun = quoteFun)
  }

  private def generateCodeForSingleElement(tname: c.TermName, tpt: c.Tree, annotations: RdlAnnotations): ArgumentContext = {
    val nextAddress = TermName(s"$tname$nextAddressSuffix")
    val Select(Ident(packageTermName: TermName), classTypeName: TypeName) = tpt

    /**
      * In case of '@' rdl modifier, we have the current scope address - it is simply referenced
      * by its name - 'address'
      */
    def quoteFun(prevAddress: TermName, continuation: c.Tree) =
      q"""
        val ($nextAddress, ${tname.toTermName}) = {
          val child = $packageTermName.${classTypeName.toTermName}(
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
  private def toRdlAnnotations(lt: List[c.Tree]): RdlAnnotations =
    lt.foldLeft(RdlAnnotations(None, None, None)) {
      /*op: (Tree, mods) => B)*/
      case (a @ RdlAnnotations(None, _, _, _), q"${ofSize: OfSize}") => a.copy(ofSize = Some(ofSize))
      case (a @ RdlAnnotations(_, None, _, _), q"${at: At}") => a.copy(at = Some(at))
      case (a @ RdlAnnotations(_, _, None, _), q"${modulo: Modulo}") => a.copy(modulo = Some(modulo))
      case (a, q"${inc: Increment}") => a.copy(increment = inc)
      case (_, other) => c.abort(other.pos, "Found unexpected annotation")
    }

  // TODO: get rid of (Address | AddressRange) hardcodes, maybe more validation
  def generateApplyMethod(tpname: c.Name, params: List[ValDef]): c.Tree = {
    val (_, otherParams) = params match {
      case (h @ q"$_ val range: AddressRange = $expr") :: th :: t  => (h, th :: t)
      case _ => c.abort(wrappingPos(params), "This macro expects that case class' constructor is of following shape: (range: AddressRange, ...)")
    }

    val results = otherParams.map {
      case q"${Modifiers(_,_,annotations)} val $tname: $tpt = $_" =>
        val annots = toRdlAnnotations(annotations)
        // TODO: naive condition...
        val codeGenerator = if (annots.ofSize.isDefined) generateCodeForList _ else generateCodeForSingleElement _
        codeGenerator(tname,tpt,annots)
      case other =>
        c.abort(other.pos, "Unknown element. Expected case class' constructor argument here.")
    }

    val lastAddressName = results.last.nextAddressName
    val constructorArgs = q"AddressRange(address, $lastAddressName)" :: results.map(r => q"${r.argumentName}")
    val creationExpression = q"${tpname.toTermName}(..$constructorArgs)"

    val resultRecipt: TermName => c.Tree =
      results.foldRight((_: TermName) => creationExpression){
        case (ArgumentContext(nextAddrName, _, quoteFun), continuation) =>
          termName => quoteFun(termName, continuation(nextAddrName))
      }

    q"""
       def apply(address: Address): ${tpname.toTypeName} = {
         ..${resultRecipt(TermName("address"))}
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
         $objMods object $objName extends { ..$objEarlyDefs} with ..$objParents { $objSelf =>
           ${generateApplyMethod(tpname, ctorParams.head)}
           ..$objDefs
         }
         """
      case (classDef @ q"$_ class ${tpname: c.Name}[..$_] $_(...$ctorParams) extends { ..$_ } with ..$_ { $_ => ..$_ }")
        :: Nil =>
        val name = tpname.toTermName
        q"""
         $classDef
         object $name {
            ${generateApplyMethod(tpname, ctorParams.head)}
         }
         """
      case other => c.abort(wrappingPos(other), "Only case class declarations can be annotated with @Initialize")

    }
    c.Expr[Any](output)
  }
}
