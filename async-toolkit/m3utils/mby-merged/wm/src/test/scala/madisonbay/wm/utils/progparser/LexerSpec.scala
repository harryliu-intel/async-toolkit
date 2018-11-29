package madisonbay.wm.utils.progparser

import org.scalatest.{FlatSpec, Matchers}

//scalastyle:off
class LexerSpec extends FlatSpec with Matchers {

  "Lexer" should "set proper regexp" in {
    import Lexer._

    val matchStr = (str: String) => fullyMatch regex str
    val notMatchStr = (str: String) => not (matchStr (str))


    "'b????_??0" should matchStr (NumBinPat)
    "'b11??_??0_010101_????" should matchStr (NumBinPat)
    "'b11_0_010101_" should matchStr (NumBinPat)
    "'b???????" should matchStr (NumBinPat)

    "'b12??_??0_010101_????" should notMatchStr (NumBinPat)
    "'b111_?.0_010101_????" should notMatchStr (NumBinPat)
    "'b111_?0_01b101_????" should notMatchStr (NumBinPat)
    "b11??_??0_010101_????" should notMatchStr (NumBinPat)
    "'bb11??_?0_010101_????" should notMatchStr (NumBinPat)
    "'b11??_?.0_010101_????" should notMatchStr (NumBinPat)
    "''b11??_??0_010101_????" should notMatchStr (NumBinPat)
  }

}
