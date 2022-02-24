MODULE TestParse EXPORTS Main;
IMPORT Stdio;
IMPORT libertyLex, libertyLexStd;
IMPORT libertyParseStd;
IMPORT Debug;
IMPORT TextWr;

VAR
  rd    := Stdio.stdin;
  lexer := NEW(libertyLexStd.T);
  parser:= NEW(libertyParseStd.T);
BEGIN
  EVAL lexer.setRd(rd);
  EVAL parser.setLex(lexer);

  EVAL parser.parse();

  WITH wr = NEW(TextWr.T).init() DO
    parser.val.write(wr);
    Debug.Out("Parsed:\n" & TextWr.ToText(wr))
  END
  
END TestParse.
