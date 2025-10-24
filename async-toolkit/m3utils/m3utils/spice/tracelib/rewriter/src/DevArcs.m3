(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE DevArcs;
IMPORT Text, Word;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN
    RETURN Text.Equal(a.pfx, b.pfx) AND Text.Equal(a.sfx, b.sfx)
  END Equal;

PROCEDURE Hash(READONLY a : T) : Word.T =
  BEGIN
    RETURN Word.Plus(Text.Hash(a.pfx), Text.Hash(a.sfx))
  END Hash;

BEGIN END DevArcs.
