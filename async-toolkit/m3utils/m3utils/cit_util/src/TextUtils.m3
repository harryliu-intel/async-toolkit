(*                                                                           *)
(*  TextUtils.m3                                                             *)
(*                                                                           *)
(*  Some useful text processing routines for the PL1 compiler.               *)
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

MODULE TextUtils;
IMPORT IntList, ScanList;
IMPORT TextReader;
IMPORT Text;
IMPORT Fmt;
IMPORT TextSet, TextSetDef, TextList;
IMPORT Lex, FloatMode;

PROCEDURE CountCharOccurences(in: TEXT; c: CHAR): CARDINAL =
  VAR
    count := 0;
  BEGIN
    FOR i := 0 TO Text.Length(in) - 1 DO
      IF Text.GetChar(in,i) = c THEN
        INC(count);
      END;
    END;
    RETURN count;
  END CountCharOccurences;

PROCEDURE ReplaceChar(in : TEXT; old, new : CHAR) : TEXT =
  VAR
    res := NEW(REF ARRAY OF CHAR, Text.Length(in));
  BEGIN
    FOR i := 0 TO Text.Length(in) - 1 DO
      WITH char = Text.GetChar(in,i) DO
        IF char = old THEN res[i] := new ELSE res[i] := char END
      END
    END;
    RETURN Text.FromChars(res^)
  END ReplaceChar;

PROCEDURE Replace(in, old, new : TEXT) : TEXT =
  VAR 
    startpos := 0;
    nextpos : CARDINAL;
  BEGIN
    WHILE FindSub(in, old, nextpos, startpos) DO
      in := Text.Sub(in, 0, nextpos) & new & 
                 Text.Sub(in, nextpos + Text.Length(old));
      startpos := nextpos + Text.Length(old) - Text.Length(new)
    END;
    RETURN in
  END Replace;

(* find first occurrence of sub in in *)
(* not a good algorithm: if necessary, code up Knuth-Morris-Pratt instead. *)
PROCEDURE FindSub(in, sub : TEXT; VAR pos : CARDINAL; start := 0) : BOOLEAN =
  VAR
    inA := NEW(REF ARRAY OF CHAR, Text.Length(in));
    subA := NEW(REF ARRAY OF CHAR, Text.Length(sub));
  BEGIN
    Text.SetChars(inA^,in);
    Text.SetChars(subA^,sub);
    FOR i := start TO LAST(inA^) - LAST(subA^) DO
      VAR
        success := TRUE;
      BEGIN
        FOR j := 0 TO LAST(subA^) DO
          IF subA[j] # inA[i + j] THEN 
            success := FALSE; 
            EXIT 
          END
        END;
        IF success THEN pos := i; RETURN TRUE END
      END
    END;
    RETURN FALSE
  END FindSub;

PROCEDURE FindAnyChar(in: TEXT; c: SET OF CHAR;
                      VAR pos: CARDINAL; start := 0): BOOLEAN =
  BEGIN
    WHILE start < Text.Length(in) DO
      IF Text.GetChar(in, start) IN c THEN pos := start; RETURN TRUE END;
      INC(start);
    END;
    RETURN FALSE;
  END FindAnyChar;

PROCEDURE HaveSub(in, sub : TEXT) : BOOLEAN = 
  VAR x : CARDINAL; BEGIN RETURN FindSub(in, sub, x) END HaveSub;


PROCEDURE HavePrefix(in, prefix: TEXT): BOOLEAN =
  BEGIN
    RETURN Text.Equal(Text.Sub(in, 0, Text.Length(prefix)), prefix);
  END HavePrefix;

PROCEDURE HaveSuffix(in, suffix: TEXT): BOOLEAN =
  VAR
    pos := Text.Length(in) - Text.Length(suffix);
  BEGIN
    RETURN pos >= 0 AND Text.Equal(Text.Sub(in, pos), suffix);
  END HaveSuffix;

PROCEDURE RemovePrefix(in, prefix: TEXT): TEXT =
  BEGIN
    <* ASSERT Text.Equal(Text.Sub(in, 0, Text.Length(prefix)),prefix) *>
    RETURN Text.Sub(in, Text.Length(prefix));
  END RemovePrefix;

PROCEDURE RemoveSuffix(in, suffix: TEXT): TEXT =
  VAR
    pos := Text.Length(in) - Text.Length(suffix);
  BEGIN
    <* ASSERT pos >= 0 AND Text.Equal(Text.Sub(in, pos), suffix) *>
    RETURN Text.Sub(in, 0, pos);
  END RemoveSuffix;

PROCEDURE Pluralize(noun : TEXT; n : INTEGER; 
                    ending : TEXT; printNum : BOOLEAN) : TEXT =
  VAR
    res : TEXT;
  BEGIN 
    IF printNum THEN res := Fmt.Int(n) & " " ELSE res := "" END;
    IF n = 1 THEN RETURN res & noun ELSE RETURN res & noun & ending END 
  END Pluralize;

PROCEDURE ListToSet(l : TextList.T) : TextSet.T =
  VAR
    res := NEW(TextSetDef.T).init();
  BEGIN
    WHILE l # NIL DO EVAL res.insert(l.head); l := l.tail END;
    RETURN res
  END ListToSet;
  
PROCEDURE SetToList(set : TextSet.T) : TextList.T =
  VAR
    iter := set.iterate();
    t : TEXT;
    res : TextList.T := NIL;
  BEGIN
    WHILE iter.next(t) DO res := TextList.Cons(t,res) END;
    RETURN res
  END SetToList;

PROCEDURE Shatter(t: TEXT; delims:="\t "; endDelims:="\n;#%";
                  skipNulls:=TRUE): TextList.T =
  BEGIN
    RETURN NEW(TextReader.T).init(t).shatter(delims, endDelims, skipNulls);
  END Shatter;


PROCEDURE ShatterInts(t: TEXT; defaultBase := 10;
                      delims := ":.\t, "; endDelims := "\n;#%"): IntList.T = 
  <* FATAL Lex.Error, FloatMode.Trap *>
  BEGIN
    RETURN ScanList.Int(Shatter(t, delims, endDelims), defaultBase);
  END ShatterInts;

PROCEDURE Filter(in: TEXT; keep: SET OF CHAR): TEXT =
  VAR
    result := NEW(REF ARRAY OF CHAR, Text.Length(in));
    len := 0;
    last := Text.Length(in) - 1;
    c: CHAR;
  BEGIN
    FOR i := 0 TO last DO
      c := Text.GetChar(in, i);
      IF c IN keep THEN
        result[len] := c;
        INC(len);
      END;
    END;
    RETURN Text.FromChars(SUBARRAY(result^, 0, len));
  END Filter;

PROCEDURE FilterOut(in: TEXT; remove := SET OF CHAR{' ', '\t', '\n'}): TEXT =
  BEGIN
    RETURN Filter(in, SET OF CHAR{FIRST(CHAR) .. LAST(CHAR)} - remove);
  END FilterOut;

PROCEDURE FilterEdges(in: TEXT; remove := SET OF CHAR{' ', '\t', '\n'}): TEXT =
  VAR
    i := 0;
  BEGIN
    LOOP
      IF i = Text.Length(in) THEN RETURN ""; END;
      IF NOT Text.GetChar(in, i) IN remove THEN EXIT END;
      INC(i);
    END;
    FOR j := Text.Length(in)-1 TO 0 BY -1 DO
      IF NOT Text.GetChar(in, j) IN remove THEN
        RETURN Text.Sub(in, i, j-i+1);
      END;
    END;
    <* ASSERT FALSE *>
  END FilterEdges;

BEGIN END TextUtils.
