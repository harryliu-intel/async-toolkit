(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Power3;

TYPE
  T = RECORD
    dynP, lkgP, totP : LONGREAL;
  END;

PROCEDURE DebugFmt(READONLY a : T) : TEXT;

CONST Brand = "Power3";

END Power3.
