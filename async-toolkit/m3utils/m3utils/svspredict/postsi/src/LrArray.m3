(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE LrArray;
IMPORT Word, LongrealType;

PROCEDURE Equal(a, b : T) : BOOLEAN =
  BEGIN RETURN a = b OR a^ = b^ END Equal;

PROCEDURE Hash(a : T) : Word.T =
  VAR
    res : Word.T := 0;
  BEGIN
    FOR i := FIRST(a^) TO LAST(a^) DO
      res := Word.Plus(res, LongrealType.Hash(a[i]))
    END;
    RETURN res
  END Hash;

BEGIN END LrArray.
