(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE QuadCheckpoint;
IMPORT LRVectorMRVTbl;
IMPORT LRVectorLRTbl;
IMPORT PointResult;

TYPE
  T = OBJECT
    iter     : CARDINAL;
    values   : LRVectorMRVTbl.T;
    fvalues  : LRVectorLRTbl.T;
    pr       : PointResult.T;
    rho      : LONGREAL;
  END;

CONST Brand = "QuadCheckpoint";

END QuadCheckpoint.
