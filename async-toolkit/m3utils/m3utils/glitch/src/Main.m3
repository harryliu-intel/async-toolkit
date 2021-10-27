MODULE Main;
IMPORT glitchParseStd, glitchLexStd;
IMPORT Stdio;
IMPORT Glitch;

VAR
  lexer := NEW(glitchLexStd.T);
  parser := NEW(glitchParseStd.T);
BEGIN
  EVAL lexer.setRd(Stdio.stdin);
  EVAL parser.setLex(lexer);

  EVAL parser.parse();

  Glitch.RunChecks()
  
END Main.
