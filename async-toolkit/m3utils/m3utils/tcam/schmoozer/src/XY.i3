(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE XY;
IMPORT LongRealSeq AS P;

TYPE
  T = P.T;

CONST Brand = "XY";

CONST Equal : PROCEDURE(READONLY a, b : T) : BOOLEAN = NIL;

END XY.
