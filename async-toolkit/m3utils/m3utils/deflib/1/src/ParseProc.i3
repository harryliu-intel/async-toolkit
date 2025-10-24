(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE ParseProc;
IMPORT DefFormat;
IMPORT ParseError;

TYPE T = PROCEDURE(t              : DefFormat.T;
                   context        : REFANY)
  RAISES { ParseError.E };

CONST Brand = "ParseProc";

END ParseProc.
