MODULE Main;
IMPORT glitchParseStd, glitchLexStd;
IMPORT Stdio;
IMPORT Glitch;
IMPORT Wr;
IMPORT Process;

VAR
  lexer := NEW(glitchLexStd.T);
  parser := NEW(glitchParseStd.T);
BEGIN
  EVAL lexer.setRd(Stdio.stdin);
  EVAL parser.setLex(lexer);

  EVAL parser.parse();

  IF Glitch.RunChecks() THEN
    Wr.PutText(Stdio.stderr, "No glitches detected\n");
    Process.Exit(0)
  ELSE
    Wr.PutText(Stdio.stderr, "GLITCHES DETECTED\n");
    Process.Exit(1)
  END
  
END Main.
