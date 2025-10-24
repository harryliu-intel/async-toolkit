(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE RMath;
IMPORT Math;

PROCEDURE sqrt(x : REAL) : REAL =
  BEGIN
    RETURN FLOAT(Math.sqrt(FLOAT(x, LONGREAL)), REAL)
  END sqrt;

BEGIN END RMath.
