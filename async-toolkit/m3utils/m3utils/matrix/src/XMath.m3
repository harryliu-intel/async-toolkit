(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE XMath;
IMPORT Math;

PROCEDURE sqrt(x : EXTENDED) : EXTENDED =
  BEGIN
    RETURN FLOAT(Math.sqrt(FLOAT(x, LONGREAL)), EXTENDED)
  END sqrt;

BEGIN END XMath.
