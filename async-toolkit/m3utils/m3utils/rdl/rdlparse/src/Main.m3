MODULE Main;
IMPORT rdlLexExt;
IMPORT rdlParseExt;
IMPORT Stdio;

VAR
  lexer := NEW(rdlLexExt.T);
  parser := NEW(rdlParseExt.T);
  rd := Stdio.stdin;
BEGIN
  EVAL lexer.setRd(rd);
  EVAL parser.setLex(lexer).parse();
END Main.
