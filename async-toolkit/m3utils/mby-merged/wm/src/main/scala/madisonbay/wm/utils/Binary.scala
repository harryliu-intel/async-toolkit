package madisonbay.wm.utils

//import language.experimental.macros

object Binary {

  /*def bin(binary: String): Int = macro BinaryMacro.impl

  object BinaryMacro {

    def impl(c: scala.reflect.macros.whitebox.Context)(binary: c.Expr[String]): c.Expr[Int] = {
      import c.universe._
      val result = binary.tree match {

        case q"${bnumber: String}" if bnumber.matches("[0-1]+") =>
          val n = Integer.parseInt(bnumber, 2)
          q"$n"

        case tree => c.error(tree.pos, "Binary number should be String [0-1]+")
          tree
      }

      c.Expr[Int](result)
    }

  }*/

  implicit class BinaryInterpolator(val sc: StringContext) extends AnyVal {

    private def bin(binary: String): Int = Integer.parseInt(binary, 2)

    def b(args: Any*): Int = {
      val orig = sc.s(args: _*)
      bin(orig)
    }

  }

}

