(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE ZeroOneX;

PROCEDURE FromBool(bool : BOOLEAN) : T =
  BEGIN
    IF bool THEN RETURN T.V1 ELSE RETURN T.V0 END
  END FromBool;

PROCEDURE Format(t : T) : TEXT =
  BEGIN RETURN Names[t] END Format;

BEGIN END ZeroOneX.
