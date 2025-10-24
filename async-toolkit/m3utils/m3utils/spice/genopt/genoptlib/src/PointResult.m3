(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE PointResult;
FROM GenOptUtils IMPORT FmtP;
FROM Fmt IMPORT F, LongReal, Bool;

PROCEDURE Format(READONLY a : T) : TEXT =
  BEGIN
    RETURN F("{ %s ; metric %s ; quadratic %s }",
             FmtP(a.p), LongReal(a.metric), Bool(a.quadratic))
  END Format;

BEGIN END PointResult.
