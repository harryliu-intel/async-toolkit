package madisonbay.wm.utils.progparser

import org.scalatest.{FlatSpec, Matchers}

//scalastyle:off
class LexerSpec extends FlatSpec with Matchers {

  "Lexer" should "set proper regexp" in {

    "'b????_??0".matches(Lexer.NumBinPat) shouldEqual true
    "'b11??_??0_010101_????".matches(Lexer.NumBinPat) shouldEqual true
    "'b11_0_010101_".matches(Lexer.NumBinPat) shouldEqual true
    "'b???????".matches(Lexer.NumBinPat) shouldEqual true

    "'b12??_??0_010101_????".matches(Lexer.NumBinPat) shouldEqual false
    "'b111_?.0_010101_????".matches(Lexer.NumBinPat) shouldEqual false
    "'b111_?0_01b101_????".matches(Lexer.NumBinPat) shouldEqual false
    "b11??_??0_010101_????".matches(Lexer.NumBinPat) shouldEqual false
    "'bb11??_?0_010101_????".matches(Lexer.NumBinPat) shouldEqual false
    "'b11??_?.0_010101_????".matches(Lexer.NumBinPat) shouldEqual false
    "''b11??_??0_010101_????".matches(Lexer.NumBinPat) shouldEqual false
  }

}
