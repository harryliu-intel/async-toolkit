(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE LibData;
IMPORT Word;
IMPORT Text;

PROCEDURE Hash(a : T) : Word.T =
  VAR
    res : Word.T := TYPECODE(a);
  BEGIN
    TYPECASE a OF
      LibRef(lr) => INC(res, Text.Hash(lr.nm))
    |
      ModelRef(mr) =>
      res := Word.Plus(res, Text.Hash(mr.type));
      res := Word.Plus(res, Text.Hash(mr.nm))
    ELSE
      (* skip *)
    END;

    RETURN res
  END Hash;
  
BEGIN END LibData.
