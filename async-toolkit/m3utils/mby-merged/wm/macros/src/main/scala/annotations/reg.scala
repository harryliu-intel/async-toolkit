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

class RegImpl(val c: Context) {
  import c.universe._

  implicit val unliftContinuousRange = Unliftable[Range] {
    case q"${start: Int} to ${end: Int}" => start to end
    case q"${start: Int} until ${end: Int}" => start until end
  }

  implicit val liftContinuousRange = Liftable[Range] { r => q"(${r.start} to ${r.last})" }

  def evalExpr[T](tree: c.Tree): T = c.eval[T](c.Expr(tree))

  def compAbort(pos: c.Position, str: String): Nothing = c.abort(pos, str)

  def compError(pos: c.Position, str: String): Unit = c.error(pos, str)

  def compInfo(pos: c.Position, str: String, flag: Boolean = false): Unit = c.info(pos, str, flag)

  def compWarn(pos: c.Position, str: String): Unit = c.warning(pos, str)

  def compAssert(pos: c.Position, cond: Boolean, str: => String): Unit = if (!cond) { compAbort(pos, str) }

  def extractClassParts(classDecl: ClassDef): (TypeName, Seq[c.Tree]) = classDecl match {
    case q"class $name { ..$body }" => (name, body)
  }

  /** Shortcut for Context capture. */
  def parseField(name: c.TermName, args: Seq[c.Tree]): FieldInfo = {
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
    def apply(name: String, args: Seq[c.Tree]): FieldInfo = {
      val (defaultHard, defaultSoft) = ("R+W", "R+W")
      val pos = wrappingPos(args.toList)
      val (range, tail) = args match {
        case q"${range: Range}" :: tail => (range, tail)
        case _ => compAbort(pos, "Invalid arguments for reg's field")
      }
      compAssert(pos, range.start <= range.last, s"Register field cannot have zero width, found: ${range.start} .. ${range.end}")
      //TODO: loop?
      val (hard, soft) = tail match {
        case Nil                                                       => (defaultHard, defaultSoft)
        case q"soft=${soft: String}" :: Nil                            => (defaultHard, soft)
        case q"hard=${hard: String}" :: q"soft=${soft: String}" :: Nil => (hard, soft)
        case q"${hard: String}" :: Nil                                 => (hard, defaultSoft)
        case q"${hard: String}" :: q"soft=${soft: String}" :: Nil      => (hard, soft)
        case q"${hard: String}" :: q"${soft: String}" :: Nil           => (hard, soft)
        case _ => compAbort(pos, "Invalid arguments for reg's field")
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
  class FieldData(val name: c.TermName, val range: Range, traits: Seq[String], body: Seq[c.Tree]) {
    var parents: Set[TypeName] = traits.map(x => TypeName(x)).toSet
    var defs: Map[c.TermName, c.Tree] = body.map { el =>
      el match {
        case q"$name = $value" => (TermName(name.toString), value)
        case Ident(name) => (name.toTermName, q"")  //TODO: special case, should handled separately
        case _ => compAbort(el.pos,s"unknown value in field $name: $el")
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

  def handleField(name: c.TermName, args: Seq[c.Tree], body: Seq[c.Tree], guard: AddressGuard): FieldData = {
    val info = parseField(name, args)
    val range = info.range
    val (pos, lim) = (range.start, range.last+1)
    val ar = AddressRange(Address(pos.bits), (lim-pos).bits)
    try {
      guard += (ar, name.toString)
    } catch {
      case AddressOverlap(first, second) => compError(c.enclosingPosition, s"Register cannot have overlapping fields, found: ${first.rangeString}, ${second.rangeString}")
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

    def unapply(tree: c.Tree): Option[(Alignment, List[c.Tree])] = {
      def check(): Unit =
        compAssert(pos, value.isEmpty, s"register can't have multiple $name assignments")

      def intToAlignment(value: Int): Alignment = {
        val bytes = value.bytes
        compAssert(pos, bytes.isPower, s"register must have $name = 2^N bytes")
        bytes.toAlignment
      }

      def gen(intVal: Int): Option[(Alignment, List[c.Tree])] = {
        val raw = intToAlignment(intVal)
        value = Some(raw)
        Some((raw, List(q"override def ${TermName(name)} = Alignment($intVal)")))
      }

      def parseBytes(value: c.Tree): Option[(Alignment, List[c.Tree])] = {
        check()
        val num = evalExpr[Int](value)
        gen(num)
      }

      def parseBits(value: c.Tree): Option[(Alignment, List[c.Tree])] = {
        check()
        val num = evalExpr[Int](value)
        compAssert(pos, num % 8 == 0, s"register can't have $name which not being full bytes, found: $num.bits")
        gen(num / 8)
      }

      def pos: Position = tree.pos

      val res = tree match {
        case q"$rname = $impl.bytes" if rname.toString == name => parseBytes(impl)
        case q"$rname = $impl.bits" if rname.toString == name => parseBits(impl)
        case q"$rname = $impl" if rname.toString == name => {
          impl match {
            case value @ Literal(_) => {
              compInfo(pos, s"assuming bits units for $name value ($impl.bits)", true)
              parseBits(value)
            }
            case _ => compAbort(pos,s"register's $name given value which cannot be handled, $impl")
          }
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

    def unapply(tree: c.Tree): Option[(c.TermName, Seq[c.Tree], Seq[c.Tree])] = {
      tree match {
        case q"field.$name($details)" => {
          val (args, body) = details match {
            // name(el)
            case q"${_: Range}" => (Seq(details), Seq())
            // name(el){...}
            case Apply(arg @ q"${_: Range}", body) => (Seq(arg), body)
            // name(...){...}
            case Apply(q"(..$args)", body) => (args, body)
            case _ => compAbort(details.pos, "Invalid field declaration")
          }
          fields = handleField(name, args, body, guard) :: fields
          Some((name, args, body))
        }
        // field name(...)
        case q"field.$name(..$args)" => {
          fields = handleField(name, args, Seq(), guard) :: fields
          Some((name, args, Seq()))
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
      tree match {
        case q"$pref.$pname = $pimpl" if pref.toString == name => {
          compAssert(tree.pos, !defs.contains(pname), s"register can't have multiple $name for $pname")
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
  def modifiedDeclaration(pos: Position, classDef: ClassDef): c.Tree = {
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
    compAssert(pos, guard.length > 0, s"Register must have at least one field")

    // default values:
    //   regwidth = 4 bytes
    //   accesswidth = regwidth
    val regwidth = qRegwidth.value.getOrElse(Alignment(4))
    val accesswidth = qAccesswidth.value.getOrElse(regwidth)

    compAssert(pos, regwidth >= accesswidth, s"Register must have regwidth >= accesswidth (found: ${regwidth.toBytes} < ${accesswidth.toBytes})")

    // all fields span through total width of...
    val cwidth = guard.width
    compAssert(pos, cwidth <= accesswidth.toBits, s"Register's contents are wider than its accesswidth (${accesswidth.toBits} < $cwidth)")

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
      case (classDecl: ClassDef) :: Nil => modifiedDeclaration(classDecl.pos, classDecl)
      case x => c.abort(wrappingPos(x), "Only class declarations can be annotated as \"reg\"")
    }
    c.Expr[Any](q"..$output")
  }
}