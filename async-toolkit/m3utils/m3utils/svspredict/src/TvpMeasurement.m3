(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE TvpMeasurement;
FROM Fmt IMPORT F, LongReal;

CONST LR = LongReal;

PROCEDURE Fmt(READONLY q : T) : TEXT =
  BEGIN
    RETURN F("Tvp { t = %s, v = %s, p = %s }",
             LR(q.t), LR(q.v), LR(q.p))
  END Fmt;

BEGIN END TvpMeasurement.
