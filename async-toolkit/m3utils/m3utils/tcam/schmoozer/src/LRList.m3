(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE LRList;
IMPORT Word;
IMPORT LongReal AS LongrealType;

PROCEDURE Hash(a : T) : Word.T =
  VAR
    res := 0;
  BEGIN
    WHILE a # NIL DO
      res := Word.Plus(res, LongrealType.Hash(a.head));
      a := a.tail
    END;
    RETURN res
  END Hash;

PROCEDURE Equal(a, b : T) : BOOLEAN =
  BEGIN
    WHILE a # NIL DO
      IF b = NIL THEN RETURN FALSE END;
      IF a.head # b.head THEN RETURN FALSE END;
      a := a.tail;
      b := b.tail
    END;
    (* a = NIL *)
    RETURN (b = NIL)
  END Equal;

BEGIN END LRList.
