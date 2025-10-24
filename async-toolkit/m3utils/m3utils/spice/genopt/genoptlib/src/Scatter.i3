(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Scatter;
IMPORT MultiEvalLRVector, MultiEvalLR;
IMPORT PointMetricLRVector, PointMetricLR;

PROCEDURE MultiEvalResult(READONLY in : MultiEvalLRVector.Result)
  : REF ARRAY OF MultiEvalLR.Result;

PROCEDURE PointMetric(READONLY in : PointMetricLRVector.T)
  : REF ARRAY OF PointMetricLR.T;

END Scatter.
