(*                                                                           *)
(*  Debug.m3                                                                 *)
(*                                                                           *)
(*  Debugging output and aborting the program.                               *)
(*                                                                           *)
(*  Copyright (c) 2000 California Institute of Technology                    *)
(*  All rights reserved.                                                     *)
(*  Department of Computer Science                                           *)
(*  Pasadena, CA 91125.                                                      *)
(*                                                                           *)
(*  Author: Mika Nystrom <mika@cs.caltech.edu>                               *)
(*                                                                           *)
(*  Permission to use, copy, modify, and distribute this software            *)
(*  and its documentation for any purpose and without fee is hereby          *)
(*  granted, provided that the above copyright notice appear in all          *)
(*  copies. The California Institute of Technology makes no representations  *)
(*  about the suitability of this software for any purpose. It is            *)
(*  provided "as is" without express or implied warranty. Export of this     *)
(*  software outside of the United States of America may require an          *)
(*  export license.                                                          *)
(*                                                                           *)
(* $Id$ *)

MODULE Debug;
IMPORT TextSet;
IMPORT TextSetDef;
IMPORT FileRd;
IMPORT Rd;
IMPORT BreakHere;
IMPORT Thread;
IMPORT OSError;
IMPORT Wr;
FROM Stdio IMPORT stderr;
IMPORT Env, Scan;
IMPORT FloatMode, Lex;
IMPORT Fmt;
IMPORT Process;

EXCEPTION ABORT;

PROCEDURE DebugThis(this : TEXT) : BOOLEAN =

  PROCEDURE HaveEnv(e : TEXT) : BOOLEAN = 
    BEGIN RETURN Env.Get(e) # NIL END HaveEnv;

  VAR
    env := "DEBUG" & this;
    res := HaveEnv("DEBUGEVERYTHING") AND NOT HaveEnv("NO" & env) OR 
           HaveEnv(env) AND NOT HaveEnv("DEBUGNOTHING");
  BEGIN
    IF level > 0 THEN
      Out("DebugThis: " & env & " = " & Fmt.Bool(res),0)
    END;
    RETURN res
  END DebugThis;

PROCEDURE Out(t: TEXT; minLevel : CARDINAL; cr:=TRUE) =
  BEGIN
    IF minLevel > level THEN RETURN END;
    IF debugFilter THEN
      IF triggers.member(t) THEN
        BreakHere.Please();
      END;
    END;
    t:=UnNil(t);
    IF cr THEN
      t:=t&"\n";
    END;
    outHook(t);
  END Out;

PROCEDURE S(t:TEXT;minLevel:CARDINAL;cr:=TRUE)=BEGIN Out(t,minLevel,cr);END S;

PROCEDURE Warning(t: TEXT) =
  BEGIN
    S("WARNING: " & UnNil(t), 0);
  END Warning;

PROCEDURE Error(t: TEXT) =
  BEGIN
    errHook(t);
  END Error;

PROCEDURE UnNil(text : TEXT) : TEXT =
  BEGIN IF text = NIL THEN RETURN "(NIL)" ELSE RETURN text END END UnNil;

PROCEDURE RaiseLevel(newLevel : CARDINAL) = 
  BEGIN
    IF newLevel > level THEN level := newLevel END
  END RaiseLevel;

PROCEDURE LowerLevel(newLevel : CARDINAL) = 
  BEGIN
    IF newLevel < level THEN level := newLevel END
  END LowerLevel;

PROCEDURE SetLevel(newLevel : CARDINAL) = BEGIN level := newLevel END SetLevel;
  
PROCEDURE GetLevel() : CARDINAL = BEGIN RETURN level END GetLevel;

(* outHook *)
VAR
  errHook := DefaultError;
  outHook := DefaultOut;
  outHookLevel:=-1;

PROCEDURE DefaultOut(t: TEXT) =
  BEGIN
    TRY
      Wr.PutText(stderr, t);
      Wr.Flush(stderr);
    EXCEPT ELSE END;
  END DefaultOut;

PROCEDURE DefaultError(t: TEXT) =
  BEGIN
    S("ERROR: " & UnNil(t), 0);
    Process.Exit(2);
  END DefaultError; 

PROCEDURE RegisterHook(out: OutHook; level:=0) =
  BEGIN
    <* ASSERT level#outHookLevel *>
    IF level>outHookLevel THEN
      outHookLevel:=level;
      outHook := out;
    END;
  END RegisterHook;

PROCEDURE RegisterErrorHook(err: OutHook) =
  BEGIN
    errHook := err;
  END RegisterErrorHook;

(* debugFilter *)

VAR
  level := 0;
  debugFilter := Env.Get("DEBUGFILTER")#NIL;
  triggers: TextSet.T;

BEGIN 
  VAR
    debugStr := Env.Get("DEBUGLEVEL");
  BEGIN
    TRY
      IF debugStr # NIL THEN level := Scan.Int(debugStr) END
    EXCEPT
      Lex.Error, FloatMode.Trap => 
        Error("DEBUGLEVEL set to nonsense! \"" & debugStr & "\"")
    END
  END;
  IF debugFilter THEN
    triggers := NEW(TextSetDef.T).init();
    VAR
      in: Rd.T;
      line: TEXT;
      <* FATAL Rd.Failure, Thread.Alerted *>
    BEGIN
      TRY
        in := FileRd.Open("debugfilter");
        LOOP
          line := Rd.GetLine(in);
          EVAL triggers.insert(line);
        END;
      EXCEPT
      | Rd.EndOfFile =>
      | OSError.E => Error("Can't find file `debugfilter'.");
      END;
    END;
  END;
END Debug.
