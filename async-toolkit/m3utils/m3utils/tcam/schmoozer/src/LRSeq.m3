(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE LRSeq;
IMPORT Word;
IMPORT LongReal AS LongrealType;

PROCEDURE Hash(a : T) : Word.T =
  VAR
    res := 0;
  BEGIN
    FOR i := 0 TO a.size()-1 DO
      res := Word.Plus(res, LongrealType.Hash(a.get(i)))
    END;
    RETURN res
  END Hash;

PROCEDURE Equal(a, b : T) : BOOLEAN =
  BEGIN
    IF a.size() # b.size() THEN RETURN FALSE END;
    FOR i := 0 TO a.size()-1 DO
      IF a.get(i) # b.get(i) THEN RETURN FALSE END
    END;
    RETURN TRUE
  END Equal;

BEGIN END LRSeq.
