(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE SchemaEntry;
IMPORT TextSeq;

(* this is a point in a Sweep 

   x is the independent value (parameter)

   report is all the other values, in text format
*)

TYPE
  T = RECORD
    x      : LONGREAL;
    report : TextSeq.T;
  END;

PROCEDURE CompareByX(READONLY a, b : T) : [-1..1];

CONST Compare = CompareByX;

CONST Brand = "SchemaEntry";

END SchemaEntry.
  
