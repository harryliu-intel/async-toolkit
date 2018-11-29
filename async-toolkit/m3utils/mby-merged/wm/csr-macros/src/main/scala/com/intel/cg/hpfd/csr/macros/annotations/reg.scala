//scalastyle:off
package com.intel.cg.hpfd.csr.macros.annotations

import com.intel.cg.hpfd.csr._
import com.intel.cg.hpfd.csr.macros.utils.{Control, Hygiene}
import com.intel.cg.hpfd.madisonbay.AddressGuard

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import com.intel.cg.hpfd.madisonbay.Memory._
import com.intel.cg.hpfd.madisonbay.Encode._
import com.intel.cg.hpfd.madisonbay._


/** Macro creating [[RdlRegister register classes]], used as a modifier similar to case.
  *
  * Maps pretty close to RDL as for grammar.
  *
  * Checks bit bounds, sizes and props existence.
  *
  * Example:
  * {{{
  *   @reg class small_tcam {
  *     regwidth = 64
  *     default resetValue = 0L
  *
  *     field mask(0 until 16) {
  *       resetValue = ~0L
  *       desc = "Bit mask over value"
  *     }
  *     field value(16 until 32)
  *     field ready(32 until 33) {
  *       encode = Boolean
  *       resetValue = true
  *       desc = "Whenever tcam value is ready to read"
  *     }
  *   }
  * }}}
  *
  */
@compileTimeOnly("enable macro paradise to expand macro annotations")
class reg extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro RegImpl.impl
}

class RegImpl(val c: Context) extends Control with LiftableMemory with Hygiene { self =>
  import c.universe._

  /** Lenses annotation for any generated public case class.
    *
    * Requires full import of Lenses macro.
    */
  val lens = q"""new Lenses("_")"""

  /** Unlift for continuous ranges, i.e. no {{{by}}}. */
  implicit val unliftContinuousRange = Unliftable[Range] {
    case q"${start: Int} to ${end: Int}" => start to end
    case q"${start: Int} until ${end: Int}" => start until end
  }

  /** Lifting continuous range, i.e. no {{{by}}}. */
  implicit val liftContinuousRange = Liftable[Range] { r => q"(${r.start} to ${r.last})" }

  /** Extracts @reg class declaration basic parts.
    *
    * No generic parameters allowed.
    * Type parameters are generated automatically, user-defined type parameters are not supported.
    */
  def extractClassParts(classDecl: ClassDef): (TypeName, Modifiers, List[Tree]) = {
    implicit val pos = classDecl.pos
    classDecl match {
      case q"$mods class $name { ..$body }" => (name, mods, body)
      case q"$mods class $name[..$params] { ..$body }" =>
        cAbort("reg-classes cannot have generic parameters")
      case q"$mods class $name[..$params](...$args) { ..$body }" =>
        cAbort("reg-classes cannot have type arguments")
      case _ => cAbort("reg-class declaration")
    }
  }

  /** Parses field arguments. */
  def parseField(name: TermName, args: List[Tree]): FieldInfo = {
    implicit val pos = wrappingPos(args)
    val (defaultHard, defaultSoft) = ("R+W", "R+W")
    val (range, tail) = args match {
      case q"${range: Range}" :: tail => (range, tail)
      case _ => cAbort("Invalid arguments for reg's field")
    }
    cAssert(range.start <= range.last) {
      s"Register field cannot have zero width, found: ${range.start} .. ${range.end}"
    }
    //TODO: loop?
    val (hard, soft) = tail match {
      case Nil                                                       => (defaultHard, defaultSoft)
      case q"soft=${soft: String}" :: Nil                            => (defaultHard, soft)
      case q"hard=${hard: String}" :: q"soft=${soft: String}" :: Nil => (hard, soft)
      case q"${hard: String}" :: Nil                                 => (hard, defaultSoft)
      case q"${hard: String}" :: q"soft=${soft: String}" :: Nil      => (hard, soft)
      case q"${hard: String}" :: q"${soft: String}" :: Nil           => (hard, soft)
      case _ => cAbort("Invalid arguments for reg's field")
    }
    FieldInfo(name.toString, range, hard, soft)
  }


  /** Basic information on raw parsed field data.
    * @param name RDL name
    * @param range range of offsets in bits
    * @param traits traits mixed into the type
    */
  case class FieldInfo(name: String, range: Range, traits: List[Type])
  object FieldInfo {
    /** Basic constructor from RDL-like spec.
      * @param name RDL name
      * @param range range of offsets in bits
      * @param hard hardware traits by their RDL names (e.g. hard="R", not HardwareReadable)
      * @param soft software traits by their RDL names (e.g. soft="R", not SoftwareReadable)
      * @param others other traits by their RDL name
      * @return info object
      */
    def apply(name: String, range: Range, hard: String, soft: String, others: String = ""): FieldInfo = {
      require(range.start < range.end)
      new FieldInfo(name, range, hardwareMap(hard) ::: softwareMap(soft) ::: getOthers(others))
    }

    /** Get other traits from comma-separated string-list. */
    def getOthers(others: String): List[Type] =
      if (others.isEmpty) { List() } else { others.split(",").toList.map(x => otherMap(x)) }

    /** Mapping of hardware, name->trait. */
    val hardwareMap = Map(
      "R" -> List(typeOf[HardwareReadable[_]]),
      "W" -> List(typeOf[HardwareWritable[_, _]]),
      "R+W" -> List(typeOf[HardwareReadable[_]], typeOf[HardwareWritable[_, _]])
    )

    /** Mapping of software, name->trait. */
    val softwareMap = Map(
      "" -> List[Type]()
    )

    /** Mapping of other traits, name->trait. */
    val otherMap = Map[String, Type]()
  }


  /** Field data container and handler.
    * @param name RDL name
    * @param range range of offsets in bits
    * @param traits names of traits to extend, generates AST analogue, parents
    * @param body props' code, generates AST analogue, defs
    */
  class FieldData(val parentTy: TypeName, val name: TermName, var typ: Option[Type], val range: Range, traits: List[Type], body: List[Tree]) {
    import FieldData._

    /** Parents to extend. */
    var parents: Set[Type] = traits.toSet

    /** Props as defined in RDL. */
    var props: Map[TermName, Tree] = body.map { el =>
      el match {
        case q"$name = $value" => (TermName(name.toString), value)
        case q"type $name = $value" => (TermName(name.toString), value)
        case Ident(name) => (name.toTermName, q"") //TODO: special case, should handled separately
        case _ => cAbort(el.pos, s"unknown value in field $name: $el")
      }
    }.toMap

    val parentMap = parents.flatMap(x => getProps(x)).toMap
    val map = fieldMap ++ parentMap

    /** Get prop by string name. */
    def getProp(name: String): Tree = props(TermName(name))

    /** Get prop option by string name. */
    def tryGetProp(name: String): Option[Tree] = props.get(TermName(name))

    /** Get prop by string name or return a default tree. */
    def getPropOr(name: String, tree: => Tree): Tree = props.getOrElse(TermName(name), tree)

    /** Remove prop. */
    def removeProp(name: String): Unit = props = props - TermName(name)

    /** Gets resetValue prop.
      *
      * Defaults to encoding's default.
      */
    def getResetValue: Tree = {
      val enTy = typeOf[Encode[_]]
      val ty = typ.getOrElse(typeOf[Long])
      val instTy = appliedType(enTy, ty)
      getPropOr("resetValue", q"implicitly[$instTy].default")
    }

    /** Gets raw resetValue. */
    def getRawResetValue: Tree = {
      val enTy = typeOf[Encode[_]]
      typ match {
        case Some(ty) if ty == typeOf[Long] => getResetValue
        case Some(ty) => q"implicitly[${tq"${appliedType(enTy, ty)}"}].toRaw($getResetValue)"
        case _ => cAbort("unknown type for field $name")(c.enclosingPosition)
      }
    }

    /** Gets modifiers. */
    protected def getMods(sym: Symbol): Modifiers = {
      val flags = if (sym.isAbstract) { NoFlags } else { Flag.OVERRIDE }
      val mods = Modifiers(flags)
      mods
    }

    /** Tries its best (but no guarantee!) to parse a type declaration */
    protected def asType(tree: Tree): Tree = {
      // sadly, it can't handle `#`, you have to use `type name = value` explicitly
      def inner(tree: Tree): Tree = tree match {
        case t if t.isType => t // already a type
        case Select(a, b) => tq"$a.${b.toTypeName}"  // someExpr.assocType
        case Ident(x @ TermName(_)) => Ident(x.toTypeName) // type lit
        case TypeApply(a, b) => AppliedTypeTree(asType(a), b.map(x => asType(x)))
        case _ => cAbort(tree.pos, s"Unknown how to convert to a type: ${showRaw(tree)}")
      }
      inner(tree)
    }

    protected def genSym(prop: Symbol, mods: Modifiers, k: TermName, v: Tree): Tree = prop match {
      case t: TypeSymbol => q"$mods type ${k.toTypeName} = ${asType(v)}"
      case t: TermSymbol if t.isVal => q"$mods val $k = $v"
      case t: TermSymbol if t.isVar => q"$mods var $k = $v"
      case t: MethodSymbol => q"$mods def $k = $v"
    }

    /** Is this prop special? */
    def isSpecial(name: TermName): Boolean = specialProps.contains(name)

    /** Get from map (either as a term name or a typename) */
    def mapGet(name: TermName): Option[Symbol] = map.get(name).orElse(map.get(name.toTypeName))

    /** Map contains (either as a term name or a typename) */
    def mapContains(name: TermName): Boolean = map.contains(name) || map.contains(name.toTypeName)

    /** Add defaults */
    def addDefaults(defaults: Map[TermName, Tree]): Unit = {
      defaults.foreach { case (k,v) =>
        if(!props.contains(k) && mapContains(k)) {
          props = props + (k -> v)
        }
      }
    }

    /** Add stuff default for the type, not parent. They can access field's data, e.g. name. */
    def addUniversalDefaults(): Unit = {
      var map = Map(
        TermName("encode") -> tq"Long",
        TermName("name") -> q"${name.toString}"
      )
      addDefaults(map)
    }

    /** Generates properties tree */
    def genProps: List[Tree] = {
      props.toList map {
        case (k, v) => {
          implicit val pos = v.pos
          val prop = mapGet(k) | s"Unknown field property: $k"
          genSym(prop, getMods(prop), k, v)
        }
      }
    }

    /** Takes ownership (i.e. removes) final props.
      *
      * They're bound to be treated specially (they can't be overriten anyway. */
    def takeFinalProps(): Map[TermName, Tree] = {
      val finals = props collect { case (k, v) if mapGet(k).map(_.isFinal).getOrElse(false) => (k, v) }
      props = props -- finals.keySet
      finals
    }

    /** Takes ownership (i.e. removes) of all special props. */
    def takeSpecialProps(): Map[TermName, Tree] = {
      val finals = takeFinalProps()
      val specials = props filter { case (k,_) => isSpecial(k) }
      props = props -- specials.keySet
      finals ++ specials
    }

    /** Generates field's final AST */
    def generate(): (Tree, Tree) = {
      val nameClass = name.toTypeName

      addUniversalDefaults()
      val finals = takeSpecialProps()
      val encodeTree = asType(finals(TermName("encode")))
      val (encode, enImpl) = encodeTree match {
        case tq"Raw" => {
          val enSy = symbolOf[Encode[_]].companion
          val encodeTy = typeOf[Encode[Long]]
          (typeOf[Long], q"lazy val en: $encodeTy = $enSy.encodeRaw(${range.last - range.start + 1})")
        }
        case _ => {
          val encode = c.typecheck(encodeTree, c.TYPEmode).tpe
          val encodeTy = appliedTypeTree(typeOf[Encode[_]], tq"$encode")
          (encode, q"lazy val en: $encodeTy = implicitly[$encodeTy]")
        }
      }
      typ = Some(encode)


      val fieldTy = appliedTypeTree(typeOf[RdlField[_, _]], tq"$nameClass", tq"$encode")
      val fieldCompTy = appliedTypeTree(typeOf[RdlFieldCompanion[_, _]], tq"$nameClass", tq"$encode")

      val instParents = fieldTy :: parents.map(p =>
        (p.etaExpand.typeParams.length) match {
          case 0 => tq"$p"
          case 1 => tq"${p.typeConstructor.typeSymbol}[$encode]"
          case 2 => tq"${p.typeConstructor.typeSymbol}[$nameClass, $encode]"
          case _ => cAbort(body.head.pos, s"Too many generic arguments for type: $p")
        }
      ).toList

      val compParents = fieldCompTy :: Nil

      val rangeTy = typeOf[Range]
      val parentComp = parentTy.toTermName

      val mods = Modifiers(Flag.CASEACCESSOR | Flag.PARAMACCESSOR)

      (q"""$mods val $name: $parentComp.$nameClass = $parentComp.$name()""",
       q"""
          @$lens
          case class $nameClass(value: $encode) extends ..$instParents {
            val range: $rangeTy = $range
            lazy val companion: $fieldCompTy = $name
            ..$genProps
          }
          object $name extends ..$compParents {
            def name: String = ${name.toString}
            $enImpl
            def width: Int = ${range.last - range.start + 1}
            def apply(value: $encode): $nameClass = new $nameClass(value)
           ..$genProps
          }
      """)
    }
  }
  object FieldData {
    /** Get map members of a type, name->symbol format. */
    def getProps(ty: Type): Map[Name, Symbol] = ty.members.map(x => (x.name.decodedName, x)).toMap

    /** Example (non-abstract) representant. */
    val instTy: Type = typeOf[RdlField[_, Long]]
    val fieldMap: Map[Name, Symbol] = getProps(instTy)

    /** Props to be handled non-trivially (don't generate code */
    val specialProps = Set("range", "encode", "sw", "hw").map(s => TermName(s))
  }

  /** Handle field declaration.
    *
    * Check the guard and produce field's data object.
    */
  def handleField(parentTy: TypeName, name: TermName, args: List[Tree], body: List[Tree], guard: AddressGuard[String]): (FieldData, AddressGuard[String]) = {
    implicit val cpos = wrappingPos(args)
    val info = parseField(name, args)
    val range = info.range
    val (pos, lim) = (range.start, range.last + 1)
    val ar = AddressRange(Address(pos.bits), (lim - pos).bits)
    val newGuard = guard.tryAdd(ar, name.toString) match {
      case Right(guard) => guard
      case Left(conflict) => cAbort {
        s"Register cannot have overlapping fields, found: ${ar.rangeString}, ${conflict.rangeString}"
      }
    }

    val deblockedBody = body match {
      case q"{..$li}" :: rest => li ::: rest
      case _ => body
    }

    val data = new FieldData(parentTy, name, None, range, info.traits, deblockedBody)
    (data, newGuard)
  }

  /**
    * Extractor for alignments. Parses either of the following syntaxes:
    *  * name = number.bytes
    *  * name = number.bits
    *  * name = number (will emit an info on implicit bits interpretation)
    * Aborts compilation if the alignment is encountered more than once.
    */
  case class qAlign(name: String, value: Option[Alignment] = None) {
    /** Try to parse prop assignment from tree */
    def unapply(tree: Tree): Option[(Alignment, Tree, qAlign)] = {
      implicit val pos = tree.pos

      /** Take memory unit as alignment, generate code. */
      def take(munit: MemoryUnit): (Alignment, Tree, qAlign) = {
        value.isEmpty                      | s"register can't have multiple $name assignments"
        val bytes = munit.tryBytes         | s"register can't have $name which not being full bytes, found: $munit"
        val alignment = bytes.tryAlignment | s"register must have $name = 2^N bytes"
        val newValue = Some(alignment)
        (alignment, q"override val ${TermName(name)} = $alignment", qAlign(name, newValue))
      }

      Some(tree) collect {
        case q"$rname = $impl" if rname.toString == name => {
          val munit = impl match {
            case q"${munit: MemoryUnit}" => munit
            case q"${int: Int}" => {
              cInfo(s"assuming bits units for $name value ($int.bits)")
              int.toLong.bits
            }
            case _ => cAbort(s"register's $name given value which cannot be handled, $impl}")
          }
          take(munit)
        }
      }
    }
  }

  /**
    * Extractor for fields. Accepts any of the syntaxes:
    *  * field name(range[, hard_access[, soft_access]]) [{ props }]
    *  * field name(range[, hard = hard_access][, soft = soft_access]) [{ props }]
    */
  case class qField(parentTy: TypeName,
                     var guard: AddressGuard[String] = AddressGuard[String](),
                     var fields: List[FieldData] = Nil) {

    def unapply(tree: Tree): Option[(TermName, Tree, Tree, qField)] = Option(tree) collect {
      case q"field.$name($details)" => {
        val (args, body) = details match {
          // name(el)
          case q"${_: Range}" => (List(details), List())
          // name(el){...}
          case Apply(arg@q"${_: Range}", body) => (List(arg), body)
          // name(...){...}
          case Apply(q"(..$args)", body) => (args, body)
          case _ => cAbort(details.pos, "Invalid field declaration")
        }
        val (newField, newGuard) = handleField(parentTy, name, args, body, guard)
        val newFields = newField :: fields
        (name, q"..$args", q"..$body", qField(parentTy, newGuard, newFields))
      }
      // field name(...)
      case q"field.$name(..$args)" => {
        val (newField, newGuard) = handleField(parentTy, name, args, List(), guard)
        val newFields = newField :: fields
        (name, q"..$args", q"", qField(parentTy, newGuard, newFields))
      }
    }
  }

  /** Extractor for special attributes of register itself, e.g. "default". Accepts syntax:
    * * name pname = pimpl
    */
  case class qProps(name: String, props: Map[TermName, Tree] = Map[TermName, Tree]()) {
    def unapply(tree: Tree): Option[(TermName, Tree, qProps)] = Some(tree) collect {
      case q"$pref.$pname = $pimpl" if pref.toString == name => {
        cAssert(tree.pos, !props.contains(pname), s"register can't have multiple $name for $pname")
        val newProps = props + (pname -> pimpl)
        (pname, pimpl, qProps(name, props))
      }
    }
  }


  /** Implements register serialization. */
  def serializeImpl(name: TypeName, fields: List[FieldData]): Tree = {
    val bvecCompSym = symbolOf[BitVector].companion
    val li = fields.map(_.name).foldRight(q"$bvecCompSym.empty": Tree) {
      (f,v) => q"$v | ($f.toBitVector >> $f.pos)"
    }
    q"..$li"
  }

  /** Implements register deserialization. */
  def deserializeImpl(name: TypeName, fields: List[FieldData]): Tree = {
    def extractField(field: FieldData): (Tree, Tree) = {
      val (name, pos) = (field.name, field.range.start)
      val tname = name.toTypeName
      val fname = c.freshName(TermName("_" + name.toString))
      (q"val $fname = vec.extract[$tname]($pos)", q"$fname": Tree)
    }
    val (vals, fnames) = fields.map(extractField).unzip
    val addrRangeCompSy = typeOf[AddressRange].typeSymbol.companion
    val addrCompSy = typeOf[Address].typeSymbol.companion
    val constr = q"new $name(range, ..$fnames)"
    q"..$vals; $constr"
  }

  /** Parses DSL code and generates proper Scala.
    *
    * Checks various invariants.
    */
  def modifiedDeclaration(classDef: ClassDef): Tree = {
    implicit val pos = classDef.pos
    val (name, _mods, body) = extractClassParts(classDef)
    val mods = Modifiers(_mods.flags | Flag.CASE)
                 .mapAnnotations(_ => lens :: _mods.annotations)

    case class State(tree1: Tree,
                     tree2: Tree,
                     props: qProps,
                     regwidth: qAlign,
                     accesswidth: qAlign,
                     alignment: qAlign,
                     fields: qField)

    val state0 = {
      val qRegwidth    = new qAlign("regwidth")
      val qAccesswidth = new qAlign("accesswidth")
      val qAlignment   = new qAlign("alignment")

      val qDefault = new qProps("default", Map(TermName("encode") -> tq"Raw"))

      val qRdlField = new qField(name)

      State(q"", q"", qDefault, qRegwidth, qAccesswidth, qAlignment, qRdlField)
    }

    // parse each entity
    val state: State = body.foldRight(state0) {
      (tree, state) => {
        import state._
        val changes = tree match {
          // defaults generate no code
          case state.props(_, _, props) => copy(q"", q"", props = props)

          // parse alignments
          case state.regwidth(_, code, newValue) => copy(q"", code, regwidth = newValue)
          case state.accesswidth(_, code, newValue) => copy(q"", code, accesswidth = newValue)
          case state.alignment(_, code, newValue) => copy(q"", code, alignment = newValue)

          // fields are dealt with after applying defaults etc.
          case state.fields(_, _, _, newValue) => copy(q"", q"", fields = newValue)

          // other children (e.g. defs) are left as-is
          case x => copy(q"", x)
        }
        val newTree1 = q"..${state.tree1}; ${changes.tree1}"
        val newTree2 = q"..${state.tree2}; ${changes.tree2}"
        changes.copy(tree1 = newTree1, tree2 = newTree2)
      }
    }

    val (_, companionBody) = (state.tree1, state.tree2)


    val (fields, guard) = (state.fields.fields, state.fields.guard)
    cAssert(guard.length > 0) { s"Register must have at least one field" }

    // default values:
    //   regwidth = 4 bytes
    //   accesswidth = regwidth
    val regwidth = state.regwidth.value.getOrElse(8.bytes.toAlignment)
    val accesswidth = state.accesswidth.value.getOrElse(regwidth)

    cAssert(regwidth >= accesswidth) {
      s"Register must have regwidth >= accesswidth (found: ${regwidth.toBytes} < ${accesswidth.toBytes})"
    }

    // all fields span through total width of...
    val cwidth = guard.width
    //TODO: uncomment when M3 generator gives you enough info
    /*
    cAssert(cwidth <= accesswidth.toBits) {
      s"Register's contents are wider than its accesswidth (${accesswidth.toBits} < $cwidth)"
    }
    */

    //TODO: handle address map
    val addrMap = guard.toAddrMap

    // apply defaults to fields' props
    fields.foreach(_.addDefaults(state.props.props))

    val regTy = appliedTypeTree(typeOf[RdlRegister[_]], tq"$name")
    val regCompTy = appliedTypeTree(typeOf[RdlRegisterCompanion[_]], tq"$name")

    val longTy = typeOf[Long]
    val addrTy = typeOf[Address]
    val addrRanTy = typeOf[AddressRange]
    val addrRanComp = addrRanTy.companionInst
    val bvecTy = typeOf[BitVector]

    val sname = name.toString
    val cname = name.toTermName

    val (fieldsClassImpl, fieldsCompImpl) = {
      val (a, b) = fields.map(_.generate()).unzip
      (a, b.flatMap(_.children))
    }

    val importEncodables = q"""import com.intel.cg.hpfd.madisonbay.Encode._"""
    //TODO: genOpticsLookup --- encaps
    q"""
        $mods class $name(val range: $addrRanTy, ..$fieldsClassImpl) extends $regTy {
          $importEncodables

          val companion: $regCompTy = $cname

          def fields: List[${typeOf[RdlField[_, _]]}] = List(..${fields.map(_.name)})

          def serialize: $bvecTy = ${serializeImpl(name, fields)}
        }
        object $cname extends $regCompTy {
          $importEncodables

          val name: String = $sname

          def apply(addr: $addrTy): $name = {
            val range = $addrRanComp.placeReg(addr, regwidth, alignment, Some(accesswidth))
            new $name(range = range)
          }

          def deserialize(vec: $bvecTy, range: $addrRanTy): $name = ${deserializeImpl(name, fields)}

          ..$fieldsCompImpl

          ..$companionBody
        }
     """
  }

  /** Implement @reg macro. */
  def impl(annottees: c.Expr[Any]*): c.Expr[Any] = {
    // only handles class declarations
    val input = annottees.map(_.tree).toList
    val output = input match {
      case (classDecl: ClassDef) :: Nil => modifiedDeclaration(classDecl)
      case x => c.abort(wrappingPos(x), "Only class declarations can be annotated as \"reg\"")
    }
    // println(showCode(output))
    c.Expr[Any](q"..$output")
  }
}
