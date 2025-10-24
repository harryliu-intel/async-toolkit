(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

GENERIC MODULE PointMetric(MultiEval);
IMPORT LongrealType;
FROM Fmt IMPORT F, LongReal;
FROM GenOptUtils IMPORT FmtP;

PROCEDURE Compare(READONLY a, b : T) : [-1..1] =
  BEGIN
    RETURN LongrealType.Compare(a.metric, b.metric)
  END Compare;

PROCEDURE Format(READONLY a : T) : TEXT =
  BEGIN
    RETURN F("{ metric %s ; p %s ; result %s }",
             LongReal(a.metric),
             FmtP(a.p),
             MultiEval.Format(a.result))
  END Format;

BEGIN END PointMetric.
