(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE FmtFrac;
IMPORT Fmt;

PROCEDURE Frac(x : LONGREAL; base : Fmt.Base := 10) : TEXT ;

CONST Brand = "FmtFrac";
  
END FmtFrac.
