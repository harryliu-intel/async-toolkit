(*                                                                           *)
(*  TextUtils.i3                                                             *)
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
INTERFACE TextUtils;
IMPORT TextList, TextSet, IntList;

(* replace every occurrence of "old" by "new" in "in" *)
PROCEDURE Replace(in, old, new : TEXT) : TEXT;
PROCEDURE ReplaceChar(in : TEXT; old, new : CHAR) : TEXT;
PROCEDURE CountCharOccurences(in: TEXT; c: CHAR): CARDINAL;

PROCEDURE Filter(in: TEXT; keep: SET OF CHAR): TEXT;
PROCEDURE FilterOut(in: TEXT; remove := SET OF CHAR{' ', '\t', '\n'}): TEXT;
PROCEDURE FilterEdges(in: TEXT; remove := SET OF CHAR{' ', '\t', '\n'}): TEXT;

(* find first occurrence of sub in in *)
PROCEDURE FindSub(in, sub : TEXT; VAR pos : CARDINAL; start := 0) : BOOLEAN;
PROCEDURE FindAnyChar(in: TEXT; c: SET OF CHAR;
                      VAR pos: CARDINAL; start := 0): BOOLEAN;

(* have substr? *)
PROCEDURE HaveSub(in, sub : TEXT) : BOOLEAN;
PROCEDURE HavePrefix(in, prefix: TEXT): BOOLEAN;
PROCEDURE HaveSuffix(in, suffix: TEXT): BOOLEAN;
PROCEDURE RemovePrefix(in, prefix: TEXT): TEXT;
PROCEDURE RemoveSuffix(in, suffix: TEXT): TEXT;

PROCEDURE InfixFormat(sep : TEXT; list : TextList.T; ignoreNulls := FALSE) : TEXT;

PROCEDURE Pluralize(noun : TEXT; count : INTEGER; 
                    ending := "s"; printNum := TRUE) : TEXT ;

PROCEDURE ListToSet(list : TextList.T) : TextSet.T;
PROCEDURE SetToList(set : TextSet.T) : TextList.T;

PROCEDURE Shatter(t: TEXT;
                  delims:="\t, ";
                  endDelims:="\n;#%";
                  skipNulls:=TRUE): TextList.T;
(* E.g. "TRY LOOP l:=TextUtils.Shatter(Rd.GetLine(rd)); ... "
   parses lines of words, ignoring comments. *)

PROCEDURE ShatterInts(t: TEXT;
                      defaultBase := 10;
                      delims      := ":.\t, ";
                      endDelims   := "\n;#%"): IntList.T;

PROCEDURE Capitalize(t: TEXT; uniqueSuffix := ""): TEXT;
(* capitalize "t"; if result would be equal to "t", append "uniqueSuffix". *)

PROCEDURE BreakLongLines(t: TEXT; atCol := 79): TEXT;
(* Break lines longer than "atCol" chars. "atCol=0" does nothing. *)

PROCEDURE GetLines(t: TEXT; n: INTEGER; firstBreakLongAtCol:=79): TEXT;
(* positive "n" for first "n" lines; negative for last "|n|" lines; *)

PROCEDURE Assemble(t: TextList.T; postDelim:=" "; skipLastDelim:=TRUE): TEXT;

END TextUtils.
