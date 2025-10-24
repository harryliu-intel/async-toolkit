(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Assignment;
IMPORT VarExpr;

TYPE
  T = RECORD
    tgt : TEXT;
    val : VarExpr.T;
    pct := FALSE;
  END;

CONST Brand = "Assignment";

CONST Equal : PROCEDURE(READONLY a, b : T) : BOOLEAN = NIL;

END Assignment.
  
