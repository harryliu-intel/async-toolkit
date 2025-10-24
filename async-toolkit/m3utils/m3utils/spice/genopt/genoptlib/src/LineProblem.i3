(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE LineProblem;
IMPORT LRVector;

TYPE
  T = RECORD
    dir    : LRVector.T;
    minp   : LRVector.T;
    minval : LONGREAL;
  END;

PROCEDURE Compare(READONLY a, b : T) : [-1 .. 1];
  (* compare by minval *)

CONST Brand = "LineProblem";

END LineProblem.
  
