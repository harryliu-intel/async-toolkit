(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE DefRoutingPoint;
IMPORT RecursiveParser;
FROM RecursiveParser IMPORT MustBeChar, GetChar;
FROM ParseError IMPORT E;
IMPORT DefInt, DefCard;

PROCEDURE MustBe(t : RecursiveParser.T; VAR p : T) RAISES { E } =
  BEGIN
    MustBeChar(t,'(');
    IF GetChar(t, '*') THEN
      IF p.x = Asterisk THEN
        RAISE E ("undetermined X")
      END
    ELSE
      DefInt.MustBe(t, p.x)
    END;
    IF GetChar(t, '*') THEN
      IF p.y = Asterisk THEN
        RAISE E ("undetermined X")
      END
    ELSE
      DefInt.MustBe(t, p.y)
    END;
    IF DefCard.Get(t, p.extValue) THEN END;
    MustBeChar(t,')');
  END MustBe;

PROCEDURE MustGet(t : RecursiveParser.T) : T RAISES { E } =
  VAR 
    p : T;
  BEGIN
    MustBeChar(t,'(');
    IF GetChar(t, '*') THEN
      IF p.x = Asterisk THEN
        RAISE E ("undetermined X")
      END
    ELSE
      DefInt.MustBe(t, p.x)
    END;
    IF GetChar(t, '*') THEN
      IF p.y = Asterisk THEN
        RAISE E ("undetermined X")
      END
    ELSE
      DefInt.MustBe(t, p.y)
    END;
    IF DefCard.Get(t, p.extValue) THEN END;
    MustBeChar(t,')');
    RETURN p
  END MustGet;

PROCEDURE Get(t : RecursiveParser.T; VAR p : T) : BOOLEAN RAISES { E } =
  (* a bit of a hack because of the LL(1) capability here *)
  BEGIN
    IF NOT GetChar(t, '(') THEN
      RETURN FALSE
    END;
    IF GetChar(t, '*') THEN
      IF p.x = Asterisk THEN
        RAISE E ("undetermined X")
      END
    ELSE
      DefInt.MustBe(t, p.x)
    END;
    IF GetChar(t, '*') THEN
      IF p.y = Asterisk THEN
        RAISE E ("undetermined X")
      END
    ELSE
      DefInt.MustBe(t, p.y)
    END;
    IF DefCard.Get(t, p.extValue) THEN END;
    MustBeChar(t,')');
    RETURN TRUE
  END Get;

BEGIN END DefRoutingPoint.
