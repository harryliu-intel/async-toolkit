package src.main.scala.annotations

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.api.Trees
import scala.reflect.macros.whitebox.Context
import com.intel.cg.hpfd.csr.Memory._


@compileTimeOnly("enable macro paradise to expand macro annotations")
class reg extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro RegImpl.impl
}

class RegImpl(val c: Context) extends WhiteControl with WhiteboLiftableMemory { self =>
  import c.universe._

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
  case class FieldInfo(name: String, range: Range, traits: List[String])
  object FieldInfo {
    //TODO: check if strings are all legit in a proper way
    def apply(name: String, range: Range, hard: String, soft: String, others: String = ""): FieldInfo = {
      require(range.start < range.end)
      new FieldInfo(name, range, hardwareMap(hard) ::: softwareMap(soft) ::: getOthers(others))
    }

    def getOthers(others: String): List[String] =
      if (others.isEmpty) { List() } else { others.split(",").toList.map(x => otherMap(x)) }

    val hardwareMap = Map(
      "R" -> List("HardwareReadable"),
      "W" -> List("HardwareWritable"),
      "R+W" -> List("HardwareReadable", "HardwareWritable")
    )

    val softwareMap = Map(
      "R" -> List("SoftwareReadable"),
      "W" -> List("SoftwareWritable"),
      "R+W" -> List("SoftwareReadable", "SoftwareWritable")
    )

    val otherMap = Map(
      "ROR" -> "ResetOnRead"
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
  class FieldData(val name: c.TermName, val range: Range, traits: List[String], body: List[c.Tree]) {
    var parents: Set[TypeName] = traits.map(x => TypeName(x)).toSet
    var defs: Map[c.TermName, c.Tree] = body.map { el =>
      el match {
        case q"$name = $value" => (TermName(name.toString), value)
        case Ident(name) => (name.toTermName, q"")  //TODO: special case, should handled separately
        case _ => cAbort(el.pos,s"unknown value in field $name: $el")
      }
    }.toMap

    /** Generates field's final AST */
    def generate: c.Tree = {
      val body = defs.toList.map { case (k,v) => q"override def $k = $v" }
      //TODO: resetValue should be overridable
      q"""
          val $name = new RdlField with ..$parents {
            val r = $range
            val resetValue = 0x0l
            ..$body
          }
        """
    }
  }

  def handleField(name: c.TermName, args: List[c.Tree], body: List[c.Tree], guard: AddressGuard): FieldData = {
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
    val newBody = body.flatMap {
      case q"$a; $b" => List(a, b)
      case x => List(x)
    }
    new FieldData(name, range, info.traits, newBody)
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
        (alignment, List(q"override def ${TermName(name)} = $alignment"))
      }

      val res = tree match {
        case q"$rname = $impl" if rname.toString == name => {
          val munit = impl match {
            case q"${munit: MemoryUnit}" => munit
            case q"${int: Int}" => {
              cInfo(s"assuming bits units for $name value ($int.bits)")
              int.toLong.bits
            }
            case _ => cAbort(s"register's $name given value which cannot be handled, $impl}")
          }
          Some(take(munit))
        }
        case _ => None
      }
      res
    }
  }

  /**
    * Extractor for fields. Accepts any of the syntaxes:
    *  * field name(range[, hard_access[, soft_access]]) [{ props }]
    *  * field name(range[, hard = hard_access][, soft = soft_access]) [{ props }]
    */
  object qField {
    /** Address guard of fields.
      *
      * Throws an error if overlapping is detecyted.
      * Can generate address map. */
    var guard = AddressGuard()

    /** Parsed fields' data. */
    var fields = List[FieldData]()

    def unapply(tree: c.Tree): Option[(c.TermName, List[c.Tree], List[c.Tree])] = {
      tree match {
        case q"field.$name($details)" => {
          val (args, body) = details match {
            // name(el)
            case q"${_: Range}" => (List(details), List())
            // name(el){...}
            case Apply(arg @ q"${_: Range}", body) => (List(arg), body)
            // name(...){...}
            case Apply(q"(..$args)", body) => (args, body)
            case _ => cAbort(details.pos, "Invalid field declaration")
          }
          fields = handleField(name, args, body, guard) :: fields
          Some((name, args, body))
        }
        // field name(...)
        case q"field.$name(..$args)" => {
          fields = handleField(name, args, List(), guard) :: fields
          Some((name, args, List()))
        }
        case _ => None
      }
    }
  }

  /** Extractor for special attributes of register itself, e.g. "default". Accepts syntax:
    * * name pname = pimpl
    */
  class qProps(val name: String) {
    /** Parsed props */
    var defs = Map[c.TermName, c.Tree]()
    def unapply(tree: c.Tree): Option[(c.TermName, c.Tree)] = {
      implicit val pos = tree.pos
      tree match {
        case q"$pref.$pname = $pimpl" if pref.toString == name => {
          cAssert(!defs.contains(pname)) { s"register can't have multiple $name for $pname" }
          defs = defs + (pname -> pimpl)
          Some((pname, pimpl))
        }
        case _ => None
      }
    }
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

    var qDefault = new qProps("default")

    // parse each entity
    val newBody = body.flatMap { child =>
      child match {
        // defaults generate no code
        case qDefault(name, code) => List()

        // parse alignments
        case qRegwidth(value, code)    => code
        case qAccesswidth(value, code) => code
        case qAlignment(value, code)   => code

        // fields are dealt with after applying defaults etc.
        case qField(name, args, body) => List()

        // other children (e.g. defs) are left as-is
        case _ => List(child)
      }
    }

    val (fields, guard) = (qField.fields, qField.guard)
    cAssert(guard.length > 0) { s"Register must have at least one field" }

    // default values:
    //   regwidth = 4 bytes
    //   accesswidth = regwidth
    val regwidth = qRegwidth.value.getOrElse(Alignment(4))
    val accesswidth = qAccesswidth.value.getOrElse(regwidth)

    cAssert(regwidth >= accesswidth) {
      s"Register must have regwidth >= accesswidth (found: ${regwidth.toBytes} < ${accesswidth.toBytes})"
    }

    // all fields span through total width of...
    val cwidth = guard.width
    cAssert(cwidth <= accesswidth.toBits) {
      s"Register's contents are wider than its accesswidth (${accesswidth.toBits} < $cwidth)"
    }

    //TODO: handle address map
    val addrMap = guard.toAddrMap

    // apply defaults to fields' props
    qDefault.defs.map { case (k,v) =>
      fields.map { el =>
        el.defs = el.defs + (k -> el.defs.getOrElse(k, v))
      }
    }

    q"""
        case class $name() extends RdlRegister {
          ..${newBody ++ fields.map(_.generate)}
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
    c.Expr[Any](q"..$output")
  }
}