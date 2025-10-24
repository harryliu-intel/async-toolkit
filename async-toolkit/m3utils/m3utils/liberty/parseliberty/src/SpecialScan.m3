(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE SpecialScan;
IMPORT Scan;
IMPORT Env;
IMPORT FloatMode, Lex;
IMPORT RegEx;
IMPORT Text;
IMPORT Fmt; FROM Fmt IMPORT F;
IMPORT Debug;

VAR
  zero := Env.Get("SCANZERO") # NIL;
  
PROCEDURE Int(txt: TEXT; defaultBase: [2..16] := 10): INTEGER
  RAISES {Lex.Error, FloatMode.Trap} =
  BEGIN
    IF zero THEN
      RETURN 0
    ELSE
      RETURN Scan.Int(txt, defaultBase)
    END
  END Int;

PROCEDURE LongReal(txt: TEXT): LONGREAL
  RAISES {Lex.Error, FloatMode.Trap} =
  BEGIN
    IF zero THEN
      RETURN 0.0d0
    ELSE
      RETURN Scan.LongReal(txt)
    END
  END LongReal;

<*FATAL RegEx.Error*>
VAR
  intPat := RegEx.Compile("\\(-*[0-9][0-9]*\\)");
  decPat := RegEx.Compile("\\(-*[0-9][0-9]*\\.[0-9][0-9]*\\)");
  expPat := RegEx.Compile("\\(-*[0-9][0-9]*\\.[0-9][0-9]*e[0-9][0-9]*\\)");

  pats := ARRAY [0..2] OF RegEx.Pattern { expPat, decPat, intPat };
  repl := ARRAY [0..2] OF TEXT          { "0.0e0", "0.0", "0" };
  lens := ARRAY [0..2] OF CARDINAL      {      5 ,    3 ,  1  };
  
PROCEDURE String(txt : TEXT) : TEXT =
  VAR
    mem : REF RegEx.Memory;
    start : CARDINAL := 0;
    success : BOOLEAN;
    pos : INTEGER;
    len : CARDINAL;
  BEGIN
    IF zero THEN
      REPEAT
        len := Text.Length(txt);
        success := FALSE;
        FOR i := FIRST(pats) TO LAST(pats) DO
          mem := NEW(REF RegEx.Memory);
          
          mem[1].start := 0;
          mem[1].stop := 0;

          Debug.Out(F("Searching \"%s\" start %s pat %s", txt, Fmt.Int(start), Fmt.Int(i)));
          pos := RegEx.Execute(pats[i], txt, start, len := len, mem := mem);
          IF pos # -1 THEN
            Debug.Out(F("Processing \"%s\" : found pattern %s at %s start %s stop %s",
                        txt, Fmt.Int(i), Fmt.Int(pos), Fmt.Int(mem[1].start), Fmt.Int(mem[1].stop)));
            
            txt := F("%s%s%s",
                     Text.Sub(txt, 0, mem[1].start),
                     repl[i],
                     Text.Sub(txt, mem[1].stop));

            (* update the start to match the length of the new txt *)
            start := mem[1].stop - (mem[1].stop - mem[1].start) + lens[i];

            Debug.Out(F("updating text to \"%s\", start <- %s", txt, Fmt.Int(start)));


            success := TRUE;
            EXIT
          END
        END
      UNTIL NOT success;
      RETURN txt
    ELSE
      RETURN txt
    END
  END String;
  
BEGIN END SpecialScan.
