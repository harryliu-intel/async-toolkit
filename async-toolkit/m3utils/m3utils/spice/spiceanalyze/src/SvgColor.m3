(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE SvgColor;
IMPORT Word;
FROM Fmt IMPORT Int, F;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN
    RETURN a = b
  END Equal;

PROCEDURE Hash(READONLY a : T) : Word.T =
  VAR
    x : Word.T := 0;
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      x := Word.Plus(x, Word.Times(a[i], 17 * ORD(i)))
    END;
    RETURN x
  END Hash;

PROCEDURE Format(READONLY a : T) : TEXT =
  BEGIN
    RETURN F("#%02s%02s%02s",
             Int(a[Channel.R], base := 16),
             Int(a[Channel.G], base := 16),
             Int(a[Channel.B], base := 16))
  END Format;
  
BEGIN END SvgColor.
