(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE LineProblem;
IMPORT LongrealType;

PROCEDURE Compare(READONLY a, b : T) : [-1 .. 1] =
  BEGIN
    RETURN LongrealType.Compare(a.minval, b.minval)
  END Compare;

BEGIN END LineProblem.
  
