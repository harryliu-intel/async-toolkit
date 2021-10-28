MODULE Main;

(* glitch checking program

   see Glitch.m3 for more details

   Author : Mika Nystrom <mika.nystroem@intel.com>
   October 27, 2021
*)

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

  (* first parse input *)
  EVAL parser.parse(); 

  (* then run checks *)
  IF Glitch.RunChecks() THEN
    (* success -- no glitches detected anywhere *)
    Wr.PutText(Stdio.stderr, "No glitches detected\n");
    Process.Exit(0)
  ELSE
    (* failure -- at least on glitch detected somewhere *)
    Wr.PutText(Stdio.stderr, "GLITCHES DETECTED\n");
    Process.Exit(1)
  END
  
END Main.
