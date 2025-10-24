(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE CktElement;
IMPORT Word;
IMPORT CktGraph;
FROM Fmt IMPORT F, Int;
IMPORT SpiceObject;

PROCEDURE Hash(a : T) : Word.T = BEGIN RETURN a.id END Hash;

PROCEDURE Format(a : T) : TEXT =
  BEGIN
    RETURN F("id %s obj %s", Int(a.id), SpiceObject.Format(a.src))
  END Format;

BEGIN END CktElement.
