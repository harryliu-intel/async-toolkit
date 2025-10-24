(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE FlatRule;
IMPORT PrsImpl;
IMPORT Word;

PROCEDURE Equal(a, b : T) : BOOLEAN =
  BEGIN
    RETURN a = b
  END Equal;

PROCEDURE Hash(a : T) : Word.T =
  BEGIN RETURN a.id END Hash;

BEGIN END FlatRule.
