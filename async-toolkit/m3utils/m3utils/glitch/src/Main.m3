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
IMPORT ParseParams;
IMPORT Debug;
IMPORT Pathname;
IMPORT OSError;
IMPORT Rd;
IMPORT FileRd;
IMPORT AL;
FROM Fmt IMPORT F;
IMPORT Text;

CONST TE = Text.Equal;
      
VAR
  pp      := NEW(ParseParams.T).init(Stdio.stderr);
  lexer   := NEW(glitchLexStd.T);
  parser  := NEW(glitchParseStd.T);
  ifn : Pathname.T;
  rd : Rd.T;
BEGIN
  TRY
    IF NOT pp.keywordPresent("-f") THEN
      Debug.Error("No -f")
    END;
    ifn := pp.getNext();
    IF TE(ifn, "-") THEN
      rd := Stdio.stdin
    ELSE
      TRY
        rd := FileRd.Open(ifn)
      EXCEPT
        OSError.E(x) => Debug.Error(F("can't open input %s : OSError.E : %s",
                                      ifn,
                                      AL.Format(x)))
      END
    END;
    pp.finish()
  EXCEPT
    ParseParams.Error => Debug.Error("Couldn't parse parameters")
  END;
 
  EVAL lexer.setRd(rd);
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
