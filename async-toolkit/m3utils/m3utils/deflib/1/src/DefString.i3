(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE DefString;
IMPORT RecursiveParser;
FROM ParseError IMPORT E;

TYPE T = TEXT; (* note the clash with DefIdent.T *)

PROCEDURE Get(p : RecursiveParser.T; VAR t : T) : BOOLEAN RAISES { E } ;

PROCEDURE MustBe(p : RecursiveParser.T; VAR t : T) RAISES { E };
PROCEDURE MustGet(p : RecursiveParser.T) : T RAISES { E };

CONST Brand = "DefString";

END DefString.
