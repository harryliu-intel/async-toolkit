(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Assertion;

TYPE
  T = RECORD
    nm         : TEXT;
    tm         : LONGREAL;
    minV, maxV : LONGREAL;
    offset     : LONGREAL;
  END;

CONST Brand = "Assertion";

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

END Assertion.
