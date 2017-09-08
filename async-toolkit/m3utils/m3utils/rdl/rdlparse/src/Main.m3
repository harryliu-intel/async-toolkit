MODULE Main;
IMPORT rdlLex;
IMPORT rdlParse;
IMPORT Stdio;

VAR
  lexer := NEW(rdlLex.T);
  parser := NEW(rdlParse.T);
  rd := Stdio.stdin;
BEGIN
  EVAL lexer.setRd(rd);
  EVAL parser.setLex(lexer).parse();
END Main.
