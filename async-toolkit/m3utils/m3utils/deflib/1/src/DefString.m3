(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE DefString;
IMPORT RecursiveParser;
FROM RecursiveParser IMPORT Next, S2T, BrackOrEmpty;
IMPORT RecursiveParserRep;
IMPORT DefLexer;
FROM DefLexer IMPORT Digit;
FROM ParseError IMPORT E;
FROM DefFormat IMPORT D;
IMPORT Text;

CONST DQ = '"';

PROCEDURE Get(t : RecursiveParser.T; VAR string : T) : BOOLEAN
  RAISES { E } =
  (* we could use a char buffer instead of TEXT here to reduce mem alloc *)

  (* this needs to handle multiple arcs and arraying! *)

  VAR
    ok := FALSE;
  BEGIN
    (* check its not a special character or a number *)
    IF    t.token.n = 0 THEN 
      RETURN FALSE
    ELSIF t.buff[t.token.start] IN NARROW(t.lexer,DefLexer.T).special THEN
      <*ASSERT t.token.n = 1*>
      RETURN FALSE
    ELSIF t.buff[t.token.start] # DQ THEN
      (* not a string *)
      RETURN FALSE
    ELSE
      <*ASSERT t.buff[t.token.start] = DQ AND t.buff[t.token.start + t.token.n -1 ] = DQ AND t.token.n # 1*>

      FOR i := t.token.start+1 TO t.token.start + t.token.n - 2 DO
        IF NOT t.buff[i] IN Digit THEN 
          ok := TRUE
        END
      END
    END;

    IF NOT ok THEN RETURN FALSE END;
      
    string := Text.FromChars(SUBARRAY(t.buff, t.token.start, t.token.n));
    Next(t);
    D("String"); 
    RETURN TRUE
  END Get;

PROCEDURE MustBe(t : RecursiveParser.T; VAR string : T) RAISES { E } =
  BEGIN
    IF NOT Get(t, string) THEN
      RAISE E ("DefString.MustBe: " & BrackOrEmpty(t.lately.nm) & " expected string here : " & S2T(t.buff, t.token))
    END;
  END MustBe;

PROCEDURE MustGet(t : RecursiveParser.T) : T RAISES { E } =
  VAR
    string : T;
  BEGIN
    IF Get(t, string) THEN
      RETURN string
    ELSE
      RAISE E ("DefString.MustBe: " & BrackOrEmpty(t.lately.nm) & " expected string here : " & S2T(t.buff, t.token))
    END;
  END MustGet;

BEGIN END DefString.

