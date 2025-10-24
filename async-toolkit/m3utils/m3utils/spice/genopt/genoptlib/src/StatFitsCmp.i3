(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE StatFitsCmp;
FROM StatFits IMPORT T, CmpResult;
      
PROCEDURE CompareByMeanValLikelihood(READONLY a, b : T) : CmpResult;
  (* HIGHER likelihood compares lower *)

PROCEDURE CompareByMeanAllLikelihood(READONLY a, b : T) : CmpResult;
  (* HIGHER likelihood compares lower *)

PROCEDURE CompareBySumAbsLinCoeff(READONLY a, b : T) : CmpResult;
  (* LOWER coefficient compares lower *)

END StatFitsCmp.
