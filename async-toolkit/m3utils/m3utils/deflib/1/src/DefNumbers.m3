(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE DefNumbers;
FROM DefFormat IMPORT D;
IMPORT RecursiveParser;
IMPORT RecursiveParserRep;
FROM ParseError IMPORT E;
FROM RecursiveParser IMPORT BrackOrEmpty, Next;

PROCEDURE GetCard(t : RecursiveParser.T; VAR c : CARDINAL) : BOOLEAN
  RAISES { E } =
  VAR 
    res := 0;
  BEGIN
    FOR i := t.token.start TO t.token.start+t.token.n-1 DO
      WITH num = ORD(t.buff[i])-ORD('0') DO
        IF num < 0 OR num > 9 THEN RETURN FALSE END;
        res := 10*res+num
      END
    END;
    c := res;
    Next(t);
    D("Card"); RETURN TRUE
  END GetCard;

PROCEDURE MustBeCard(t : RecursiveParser.T; VAR c : CARDINAL) RAISES { E } = 
  BEGIN
    IF NOT GetCard(t, c) THEN
      RAISE E ("MustBeCard, "&BrackOrEmpty(t.lately.nm)&" expected number")
    END
  END MustBeCard;

PROCEDURE MustGetCard(t : RecursiveParser.T) : CARDINAL RAISES { E } = 
  VAR 
    c : CARDINAL;
  BEGIN
    IF NOT GetCard(t, c) THEN
      RAISE E ("MustBeCard, "&BrackOrEmpty(t.lately.nm)&" expected number")
    END;
    RETURN c
  END MustGetCard;

PROCEDURE MustBeInt(t : RecursiveParser.T; VAR i : INTEGER) RAISES { E } =
  (* kind of ugly *)
  VAR
    j : CARDINAL;
  BEGIN
    IF GetCard(t, j) THEN
      i := j;
      RETURN 
    ELSIF t.token.n = 1 AND t.buff[t.token.start] = '+' THEN
      Next(t); 
      MustBeCard(t, j);
      i := j;
      RETURN
    ELSIF t.token.n = 1 AND t.buff[t.token.start] = '-' THEN
      Next(t); 
      IF NOT GetCard(t, j) THEN
        RAISE E("MustBeInt, "&BrackOrEmpty(t.lately.nm)&" expected number")
      END;
      i := -j;
      RETURN
    ELSE
      RAISE E("MustBeInt, "&BrackOrEmpty(t.lately.nm)&" expected integer")
    END
  END MustBeInt;

PROCEDURE MustGetInt(t : RecursiveParser.T) : INTEGER RAISES { E } =
  VAR 
    i : INTEGER;
  BEGIN
    MustBeInt(t, i);
    RETURN i
  END MustGetInt;

BEGIN END DefNumbers.
