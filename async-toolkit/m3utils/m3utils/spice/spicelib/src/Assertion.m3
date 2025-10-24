(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE Assertion;
IMPORT Text;

CONST TE = Text.Equal;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN 
    RETURN TE(a.nm,b.nm) AND a.tm = b.tm AND a.minV = b.minV AND a.maxV = b.maxV
  END Equal;

BEGIN END Assertion.
