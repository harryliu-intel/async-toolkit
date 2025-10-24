(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

GENERIC INTERFACE PointMetric(MultiEval);
IMPORT LRVector;

TYPE
  T = RECORD
    metric : LONGREAL;
    p      : LRVector.T;
    result : MultiEval.Result;
  END;

CONST Brand = "PointMetric(" & MultiEval.Brand & ")";

PROCEDURE Compare(READONLY a, b : T) : [-1..1];

PROCEDURE Format(READONLY a : T) : TEXT;

END PointMetric.
