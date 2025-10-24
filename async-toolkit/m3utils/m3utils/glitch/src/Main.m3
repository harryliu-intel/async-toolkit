(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

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
FROM Fmt IMPORT F, Int;
IMPORT Text;
IMPORT Thread;

CONST TE = Text.Equal;

<*FATAL Thread.Alerted, Wr.Failure*>
      
VAR
  pp      := NEW(ParseParams.T).init(Stdio.stderr);
  lexer   := NEW(glitchLexStd.T);
  parser  := NEW(glitchParseStd.T);
  ifn : Pathname.T;
  rd : Rd.T;
  asyncLimit := LAST(CARDINAL);
  status : CARDINAL;
  alg1MaxDepth : CARDINAL := 127;
BEGIN
  TRY
    IF pp.keywordPresent("-limit") THEN
      asyncLimit := pp.getNextInt()
    END;
    IF pp.keywordPresent("-maxdepth") THEN
      alg1MaxDepth := pp.getNextInt()
    END;
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

  Wr.PutText(Stdio.stderr, "START GLITCH SEARCH " & ifn & "\n");

  EVAL lexer.setRd(rd);
  EVAL parser.setLex(lexer);

  (* first parse input *)
  EVAL parser.parse(); 

  (* then run checks *)
  TRY
    IF Glitch.RunChecks(asyncLimit, alg1MaxDepth) THEN
      (* success -- no glitches detected anywhere *)
      Wr.PutText(Stdio.stderr, "No glitches detected\n");
      status := 0
    ELSE
      (* failure -- at least on glitch detected somewhere *)
      Wr.PutText(Stdio.stderr, "GLITCHES DETECTED\n");
      status := 1
    END
  EXCEPT
    Glitch.Timeout =>
    Wr.PutText(Stdio.stderr, "GLITCHES TIMEOUT\n");
    status := 2
  END;
  
  Wr.PutText(Stdio.stderr, F("COMPLETED GLITCH SEARCH %s STATUS %s\n",
                             ifn, Int(status)));

  Process.Exit(status)
  
END Main.
