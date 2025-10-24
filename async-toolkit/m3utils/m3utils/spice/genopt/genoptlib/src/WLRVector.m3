(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE WLRVector;

FROM Fmt IMPORT LongReal, F;
FROM GenOptUtils IMPORT FmtP;

PROCEDURE Format(READONLY a : T) : TEXT =
  BEGIN
    RETURN F("{ %s y=%s w=%s }",
             FmtP(a.v), LongReal(a.y), LongReal(a.w))
  END Format;

BEGIN END WLRVector.
