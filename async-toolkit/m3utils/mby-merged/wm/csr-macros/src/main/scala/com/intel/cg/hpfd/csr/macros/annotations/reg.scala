//scalastyle:off
package com.intel.cg.hpfd.csr.macros.annotations

import com.intel.cg.hpfd.csr._
import com.intel.cg.hpfd.csr.macros.MacroUtils.{Control, HygienicUnquote}
import com.intel.cg.hpfd.madisonbay.{AddressGuard, AddressOverlap}

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import com.intel.cg.hpfd.madisonbay.Memory._
import com.intel.cg.hpfd.madisonbay._


@compileTimeOnly("enable macro paradise to expand macro annotations")
class reg extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro RegImpl.impl
}

class RegImpl(val c: Context) extends Control with LiftableMemory with HygienicUnquote { self =>
  import c.universe._

  // for implicit revolution
  val imports = c.typecheck(q"""
    // import _root_.com.intel.cg.hpfd.madisonbay.Encode._
  """)

  implicit val unliftContinuousRange = Unliftable[Range] {
    case q"${start: Int} to ${end: Int}" => start to end
    case q"${start: Int} until ${end: Int}" => start until end
  }

  implicit val liftContinuousRange = Liftable[Range] { r => q"(${r.start} to ${r.last})" }


  def evalExpr[T](tree: c.Tree): T = c.eval[T](c.Expr(tree))


  def extractClassParts(classDecl: ClassDef): (TypeName, List[c.Tree]) = classDecl match {
    case q"class $name { ..$body }" => (name, body)
  }

  /** Shortcut for Context capture. */
  def parseField(name: c.TermName, args: List[c.Tree]): FieldInfo = {
    val fparse = new FieldParser
    fparse(name.toString, args)
  }


  /** Basic information on raw parsed field data.
    * @param name
    * @param range range of offsets in bits
    * @param traits by their RDL-like names (e.g. hard="R", not HardwareReadable)
    */
  case class FieldInfo(name: String, range: Range, traits: List[Type])
  object FieldInfo {
    //TODO: check if strings are all legit in a proper way
    def apply(name: String, range: Range, hard: String, soft: String, others: String = ""): FieldInfo = {
      require(range.start < range.end)
      new FieldInfo(name, range, hardwareMap(hard) ::: softwareMap(soft) ::: getOthers(others))
    }

    def getOthers(others: String): List[Type] =
      if (others.isEmpty) { List() } else { others.split(",").toList.map(x => otherMap(x)) }

    val hardwareMap = Map(
      "R" -> List(typeOf[HardwareReadable[_]]),
      "W" -> List(typeOf[HardwareWritable[_, _]]),
      "R+W" -> List(typeOf[HardwareReadable[_]], typeOf[HardwareWritable[_, _]])
    )

    val softwareMap = Map(
      "" -> List[Type]()
      /*
      "R" -> List(typeOf[SoftwareReadable[_]]),
      "W" -> List(typeOf[SoftwareWritable[_]]),
      "R+W" -> List(typeOf[SoftwareReadable[_]], typeOf[SoftwareWritable[_]])
      */
    )

    val otherMap = Map[String, Type](
      /*
      "ROR" -> typeOf[ResetOnRead]
      */
      //TODO: others
    )
  }


  /** Parses "field" annotation into FieldInfo. */
  class FieldParser {
    def apply(name: String, args: List[c.Tree]): FieldInfo = {
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
      FieldInfo(name, range, hard, soft)
    }
  }


  /** Field data container and handler.
    * @param name field's name
    * @param range range of offsets in bits
    * @param traits names of traits to extend, generates AST analogue, parents
    * @param body props' code, generates AST analogue, defs
    */
  class FieldData(val parentTy: TypeName, val name: TermName, var typ: Option[Type], val range: Range, traits: List[Type], body: List[c.Tree]) {
    import FieldData._

    var parents: Set[Type] = traits.toSet
    var defs: Map[c.TermName, c.Tree] = body.map { el =>
      el match {
        case q"$name = $value" => (TermName(name.toString), value)
        case q"type $name = $value" => (TermName(name.toString), value)
        case Ident(name) => (name.toTermName, q"") //TODO: special case, should handled separately
        case _ => cAbort(el.pos, s"unknown value in field $name: $el")
      }
    }.toMap

    def getDef(name: String): c.Tree = defs(TermName(name))

    def tryGetDef(name: String): Option[c.Tree] = defs.get(TermName(name))

    def getDefOr(name: String, tree: => c.Tree): c.Tree = defs.getOrElse(TermName(name), tree)

    def removeDef(name: String): Unit = defs = defs - TermName(name)

    def getResetValue: c.Tree = {
      val enTy = typeOf[Encode[_]]
      val instTy = appliedType(enTy, typeOf[Long])
      getDefOr("resetValue", q"implicitly[$instTy].default")
    }

    def getRawResetValue: c.Tree = {
      val enTy = typeOf[Encode[_]]
      typ match {
        case Some(ty) if ty == typeOf[Long] => getResetValue
        case Some(ty) => q"implicitly[${tq"${appliedType(enTy, ty)}"}].toRaw($getResetValue)"
        case _ => cAbort("unknown type for field $name")(c.enclosingPosition)
      }
    }

    protected def getMods(sym: Symbol): Modifiers = {
      import c.universe.Flag._
      var flags: FlagSet = NoFlags
      if (!sym.isAbstract) flags |= OVERRIDE
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

    val parentMap = parents.flatMap(x => getProps(x)).toMap
    val map = fieldMap ++ parentMap

    /** Is this prop special? */
    def isSpecial(name: TermName): Boolean = specialProps.contains(name)

    /** Get from map (either as a term name or a typename) */
    def mapGet(name: TermName): Option[Symbol] = map.get(name).orElse(map.get(name.toTypeName))

    /** Map contains (either as a term name or a typename) */
    def mapContains(name: TermName): Boolean = map.contains(name) || map.contains(name.toTypeName)

    /** Add defaults */
    def addDefaults(defaults: Map[TermName, Tree]): Unit = {
      defaults.foreach { case (k,v) =>
        if(!defs.contains(k) && mapContains(k)) {
          defs = defs + (k -> v)
        }
      }
    }

    /** Add stuff default for the type, not parent. They can access field's data, e.g. name. */
    def addUniversalDefaults(): Unit = {
      val map = Map(
        TermName("encode") -> tq"Long",
        TermName("name") -> q"${name.toString}"
      )
      addDefaults(map)
    }

    /** Generates properties tree */
    def genProps: List[c.Tree] = {
      defs.toList map {
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
    def takeFinalProps(): Map[TermName, c.Tree] = {
      val finals = defs collect { case (k, v) if mapGet(k).map(_.isFinal).getOrElse(false) => (k, v) }
      defs = defs -- finals.keySet
      finals
    }

    /** Takes ownership (i.e. removes) of all special props. */
    def takeSpecialProps(): Map[TermName, c.Tree] = {
      val finals = takeFinalProps()
      val specials = defs filter { case (k,_) => isSpecial(k) }
      defs = defs -- specials.keySet
      finals ++ specials
    }

    /** Generates field's final AST */
    def generate: c.Tree = {
      addUniversalDefaults()
      val finals = takeSpecialProps()
      val encodeTree = asType(finals(TermName("encode")))
      val encode = c.typecheck(encodeTree, c.TYPEmode).tpe
      typ = Some(encode)

      //val instTy = tq"${typeOf[RdlField[_, _]].typeConstructor}[$parentTy, $encode]"
      val instParents = parents.map(p =>
        (p.etaExpand.typeParams.length) match {
          case 0 => tq"$p"
          case 1 => tq"${p.typeConstructor.typeSymbol}[$encode]"
          case 2 => tq"${p.typeConstructor.typeSymbol}[$parentTy, $encode]"
          case _ => cAbort(body.head.pos, s"Too many generic arguments for type: $p")
        }
      )
      val fieldTy = appliedTypeTree(typeOf[RdlField[_, _]], tq"$parentTy", tq"$encode")
      val encodeTy = appliedTypeTree(typeOf[Encode[_]], tq"$encode")

      val parent = c.freshName(TermName("parent"))
      val rangeTy = typeOf[Range]

      q"""
        val $name = {
          ..$imports
          val $parent: $parentTy = this
          new $fieldTy with ..$instParents {
            val reg: $parentTy = $parent
            val en: $encodeTy = implicitly[$encodeTy]
            val range: $rangeTy = $range
            ..$genProps
          }
        }
      """
    }
  }
  object FieldData {
    def getProps(ty: Type): Map[Name, Symbol] = ty.members.map(x => (x.name.decodedName, x)).toMap
    protected def getMods(sym: Symbol): Modifiers = {
      import c.universe.Flag._
      var flags: FlagSet = NoFlags
      if (!sym.isAbstract) flags |= OVERRIDE
      val mods = Modifiers(flags)
      mods
    }

    /** Example (non-abstract) representant. */
    val instTy: Type = typeOf[RdlField[_, Long]]
    val fieldMap: Map[Name, Symbol] = getProps(instTy)

    /** Props to be handled non-trivially (don't generate code */
    val specialProps = Set("range", "encode", "sw", "hw").map(s => TermName(s))
  }

  def handleField(parentTy: TypeName, name: c.TermName, args: List[c.Tree], body: List[c.Tree], guard: AddressGuard[String]): FieldData = {
    implicit val cpos = wrappingPos(args)
    val info = parseField(name, args)
    val range = info.range
    val (pos, lim) = (range.start, range.last+1)
    val ar = AddressRange(Address(pos.bits), (lim-pos).bits)
    try {
      guard += (ar, name.toString)
    }
    catch {
      case AddressOverlap(first, second) => cError {
        s"Register cannot have overlapping fields, found: ${first.rangeString}, ${second.rangeString}"
      }
    }

    val deblockedBody = body match {
      case q"{..$li}" :: rest => li ::: rest
      case _ => body
    }
    new FieldData(parentTy, name, None, range, info.traits, deblockedBody)
  }

  /**
    * Extractor for alignments. Parses either of the following syntaxes:
    *  * name = number.bytes
    *  * name = number.bits
    *  * name = number (will emit an info on implicit bits interpretation)
    * Aborts compilation if the alignment is encountered more than once.
    */
  class qAlign(val name: String) {
    /** Value of the parsed alignment. */
    var value: Option[Alignment] = None

    /** Try to parse prop assignment from tree */
    def unapply(tree: c.Tree): Option[(Alignment, List[c.Tree])] = {
      implicit val pos = tree.pos

      /** Take memory unit as alignment, generate code. */
      def take(munit: MemoryUnit): (Alignment, List[c.Tree]) = {
        value.isEmpty                      | s"register can't have multiple $name assignments"
        val bytes = munit.tryBytes         | s"register can't have $name which not being full bytes, found: $munit"
        val alignment = bytes.tryAlignment | s"register must have $name = 2^N bytes"
        value = Some(alignment)
        (alignment, List(q"override val ${TermName(name)} = $alignment"))
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
  class qField(val parentTy: TypeName) {
    /** Address guard of fields.
      *
      * Throws an error if overlapping is detecyted.
      * Can generate address map. */
    var guard = AddressGuard[String]()

    /** Parsed fields' data. */
    var fields = List[FieldData]()

    def unapply(tree: c.Tree): Option[(c.TermName, List[c.Tree], List[c.Tree])] = Option(tree) collect {
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
        fields = handleField(parentTy, name, args, body, guard) :: fields
        (name, args, body)
      }
      // field name(...)
      case q"field.$name(..$args)" => {
        fields = handleField(parentTy, name, args, List(), guard) :: fields
        (name, args, List())
      }
    }
  }

  /** Extractor for special attributes of register itself, e.g. "default". Accepts syntax:
    * * name pname = pimpl
    */
  class qProps(val name: String) {
    /** Parsed props */
    var defs = Map[c.TermName, c.Tree]()
    def unapply(tree: c.Tree): Option[(c.TermName, c.Tree)] = Some(tree) collect {
      case q"$pref.$pname = $pimpl" if pref.toString == name => {
        cAssert(tree.pos, !defs.contains(pname), s"register can't have multiple $name for $pname")
        defs = defs + (pname -> pimpl)
        (pname, pimpl)
      }
    }
  }


  /** Implements resetted instance constructor. */
  def resetImpl(fields: List[FieldData]): c.Tree = {// implement generic reset
    val regTy = typeOf[RdlRegister[_]]
    val regObj = regTy.companionInst
    val stateChanges = fields.map(f => q"state = $regObj.writeState(state, ${f.range}, ${f.getRawResetValue})")
    q"""var state = 0L; ..${stateChanges}; apply(addr, state)"""
  }


  /** Parses DSL code and generates proper Scala.
    *
    * Checks various invariants.
    */
  def modifiedDeclaration(classDef: ClassDef): c.Tree = {
    implicit val pos = classDef.pos
    val (name, body) = extractClassParts(classDef)

    val qRegwidth    = new qAlign("regwidth")
    val qAccesswidth = new qAlign("accesswidth")
    val qAlignment   = new qAlign("alignment")

    val qDefault = new qProps("default")
    qDefault.defs += TermName("encode") -> tq"Long"

    val qRdlField = new qField(name)

    // parse each entity
    val (newBody, companionBody) = {
      val (a, b) = body.map { child =>
        child match {
          // defaults generate no code
          case qDefault(_, _) => (List(), List())

          // parse alignments
          case qRegwidth(_, code)    => (code, code)
          case qAccesswidth(_, code) => (code, code)
          case qAlignment(_, code)   => (code, code)

          // fields are dealt with after applying defaults etc.
          case qRdlField(_, _, _) => (List(), List())

          // other children (e.g. defs) are left as-is
          case _ => (List(child), List())
        }
      }.unzip
      (a.flatten, b.flatten)
    }

    val (fields, guard) = (qRdlField.fields, qRdlField.guard)
    cAssert(guard.length > 0) { s"Register must have at least one field" }

    // default values:
    //   regwidth = 4 bytes
    //   accesswidth = regwidth
    val regwidth = qRegwidth.value.getOrElse(8.bytes.toAlignment)
    val accesswidth = qAccesswidth.value.getOrElse(regwidth)

    cAssert(regwidth >= accesswidth) {
      s"Register must have regwidth >= accesswidth (found: ${regwidth.toBytes} < ${accesswidth.toBytes})"
    }

    // all fields span through total width of...
    // val cwidth = guard.width
    //TODO: uncomment when M3 generator gives you enough info
    /*
    cAssert(cwidth <= accesswidth.toBits) {
      s"Register's contents are wider than its accesswidth (${accesswidth.toBits} < $cwidth)"
    }
    */

    // apply defaults to fields' props
    fields.foreach(_.addDefaults(qDefault.defs))

    val regTy = appliedTypeTree(typeOf[RdlRegister[_]], tq"$name")
    val regCompTy = appliedTypeTree(typeOf[RdlRegisterCompanion[_]], tq"$name")
    val longTy = typeOf[Long]
    val addrTy = typeOf[Address]
    val addrRanTy = typeOf[AddressRange]
    val addrRanComp = addrRanTy.companionInst
    val sname = name.toString
    val cname = name.toTermName

    q"""
        @_root_.monocle.macros.Lenses("_")
        case class $name(range: $addrRanTy, state: $longTy) extends $regTy {
          ..$imports
          val name: String = $sname
          val companion: $regCompTy = $cname
          def rawCopy(range: $addrRanTy = range, state: $longTy = state): $name = new $name(range, state)
          ..${newBody ++ fields.map(_.generate)}
        }
        object $cname extends $regCompTy {
          ..$imports
          def apply(addr: $addrTy, state: $longTy): $name = {
            val range = $addrRanComp.placeReg(addr, regwidth, alignment, Some(accesswidth))
            new $name(range, state)
          }
          def apply(addr: $addrTy): $name = ${resetImpl(fields)}

          def genOpticsLookup[A](
            me: $name,
            path: _root_.monocle.Optional[A, $name]
          ): _root_.scala.collection.immutable.HashMap[Address, _root_.monocle.Optional[A, Long]] = {
            val stateOptional = _root_.monocle.Optional[$name,$longTy] {
              r => Some(r.state)
            } {
              newValue => _.copy(state = newValue)
            }
            _root_.scala.collection.immutable.HashMap(me.range.pos -> (path composeOptional stateOptional))
          }

          ..$companionBody
        }
     """
  }

  def impl(annottees: c.Expr[Any]*): c.Expr[Any] = {
    // only handles class declarations
    val input = annottees.map(_.tree).toList
    val output = input match {
      case (classDecl: ClassDef) :: Nil => modifiedDeclaration(classDecl)
      case x => c.abort(wrappingPos(x), "Only class declarations can be annotated as \"reg\"")
    }
    // println(output)
    c.Expr[Any](q"..$output")
  }
}
