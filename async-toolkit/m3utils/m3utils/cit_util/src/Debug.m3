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
IMPORT Wr, TextWr, Text;
FROM Stdio IMPORT stderr;
IMPORT Env, Scan;
IMPORT FloatMode, Lex;
IMPORT Fmt;
IMPORT Process;
IMPORT ThreadF;
IMPORT RefList;

VAR options := SET OF Options {};

PROCEDURE SetOptions(newOptions : SET OF Options) =
  BEGIN options := newOptions END SetOptions;

PROCEDURE GetOptions() : SET OF Options = BEGIN RETURN options END GetOptions;

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

VAR pidText := "(" & Fmt.Int(Process.GetMyID()) & ") ";
 
PROCEDURE Out(t: TEXT; minLevel : CARDINAL; cr:=TRUE) =
  BEGIN
    IF minLevel > level THEN RETURN END;
    IF debugFilter THEN
      IF triggers.member(t) THEN
        BreakHere.Please();
      END;
    END;

    IF Options.PrintThreadID IN options THEN
      WITH threadText = "((" & Fmt.Int(ThreadF.MyId()) & ")) " DO
        t := threadText & t
      END
    END;

    IF Options.PrintPID IN options THEN
      t := pidText & t
    END;

    t:=UnNil(t);
    IF cr THEN
      t:=t&"\n";
    END;
    outHook(t);
  END Out;

PROCEDURE HexOut(t : TEXT; minLevel : CARDINAL; cr : BOOLEAN; 
                 toHex : PROCEDURE (t : TEXT) : TEXT) =
    BEGIN Out(toHex(t), minLevel, cr) END HexOut;

PROCEDURE ToHex(t : TEXT) : TEXT =
  <* FATAL Thread.Alerted, Wr.Failure *>
  <*UNUSED*> CONST 
    BackSlash = VAL(8_134, CHAR); 
  CONST
    brax = TRUE;
  VAR
    wr := NEW(TextWr.T).init();
  CONST
    OK = SET OF CHAR { ' '..'~' }; (* the whole standard printable set *)
  BEGIN
    FOR i := 0 TO Text.Length(t)-1 DO
      WITH c = Text.GetChar(t,i) DO
        IF c IN OK THEN 
          Wr.PutChar(wr,c) 
        ELSE 
          IF brax THEN
            Wr.PutText(wr, Fmt.F("[\\%03s]",Fmt.Int(ORD(c))))
          ELSE
            Wr.PutText(wr, Fmt.F("\\%03s",Fmt.Int(ORD(c))))
          END
        END
      END
    END;
    RETURN TextWr.ToText(wr)
  END ToHex;  

PROCEDURE S(t:TEXT;minLevel:CARDINAL;cr:=TRUE)=BEGIN Out(t,minLevel,cr);END S;

PROCEDURE Warning(t: TEXT) =
  BEGIN
    S("WARNING: " & UnNil(t), 0);
  END Warning;

PROCEDURE Error(t: TEXT; exit : BOOLEAN) =
  BEGIN
    IF exit THEN
      errHook(t)
    ELSE
      S("ERROR: " & UnNil(t), 0)
    END
  END Error;

PROCEDURE Check(b : BOOLEAN; msg : TEXT; exit : BOOLEAN) =
  BEGIN IF NOT b THEN Error(msg,exit) END END Check;

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
  VAR
    p := streams;
  BEGIN
    WHILE p # NIL DO
      TRY
        Wr.PutText(p.head, t); Wr.Flush(p.head);
      EXCEPT ELSE END;
      p := p.tail
    END
  END DefaultOut;

PROCEDURE AddStream(wr : Wr.T) = 
  BEGIN streams := RefList.Cons(wr, streams) END AddStream;

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
  streams := RefList.List1(stderr);

BEGIN 
  VAR
    debugStr := Env.Get("DEBUGLEVEL");
  BEGIN
    TRY
      IF debugStr # NIL THEN level := Scan.Int(debugStr) END
    EXCEPT
      Lex.Error, FloatMode.Trap => 
        Error("DEBUGLEVEL set to nonsense! \"" & debugStr & "\"",TRUE)
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
      | OSError.E => Error("Can't find file `debugfilter'.",TRUE);
      END;
    END;
  END;
END Debug.
