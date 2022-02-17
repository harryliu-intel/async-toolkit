MODULE TestParse EXPORTS Main;
IMPORT Stdio;
IMPORT libertyLex, libertyLexStd;
IMPORT libertyParse;

VAR
  rd    := Stdio.stdin;
  lexer := NEW(libertyLexStd.T);
  parser:= NEW(libertyParse.T);
BEGIN
  EVAL lexer.setRd(rd);
  EVAL parser.setLex(lexer);

  EVAL parser.parse();
  
END TestParse.
