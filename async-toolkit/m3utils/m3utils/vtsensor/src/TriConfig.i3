(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TriConfig;
IMPORT P3;

TYPE
  T = RECORD
    corner : TEXT;
    temp   : LONGREAL;
    V      : P3.T;
    f      : P3.T;
  END;

PROCEDURE Format(t : T) : TEXT;
  
CONST Brand = "TriConfig";

CONST Equal : PROCEDURE(READONLY a, b : T): BOOLEAN = NIL;

PROCEDURE Minus(READONLY a, b : T) : T;
  
END TriConfig.

