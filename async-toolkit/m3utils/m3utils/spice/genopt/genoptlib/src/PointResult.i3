(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE PointResult;
IMPORT LRVector;

TYPE
  T = RECORD
    p         : LRVector.T;
    metric    : LONGREAL;
    quadratic : BOOLEAN;
    rho       : LONGREAL;
  END;

CONST Brand = "PointResult";

PROCEDURE Format(READONLY a : T) : TEXT;

END PointResult.
    
