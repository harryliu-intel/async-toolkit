MODULE Main;

IMPORT vnfParseStd, vnfLexStd;
IMPORT Stdio;

VAR
  lexer := NEW(vnfLexStd.T);
  parser := NEW(vnfParseStd.T);
BEGIN
  EVAL lexer.setRd(Stdio.stdin);
  EVAL parser.setLex(lexer);

  (* first parse input *)
  EVAL parser.parse(); 
END Main.
