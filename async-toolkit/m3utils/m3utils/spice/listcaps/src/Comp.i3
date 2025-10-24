(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Comp;

TYPE
  T = RECORD
    nm  : TEXT;
    val : LONGREAL;
    m   : CARDINAL := 1; (* multiplicity *)
  END;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE Compare(READONLY a, b : T) : [-1..1];
  (* compare by val *)

PROCEDURE CompareByMulVal(READONLY a, b : T) : [-1..1];
  (* compare by mult*val *)
  
CONST Brand = "Comp";

END Comp.
  
