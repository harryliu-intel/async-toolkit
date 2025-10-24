(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE DefIdent;
IMPORT RecursiveParser;
FROM ParseError IMPORT E;

TYPE T = TEXT;

PROCEDURE Get(p : RecursiveParser.T; VAR t : T) : BOOLEAN;
PROCEDURE Peek(p : RecursiveParser.T; VAR t : T) : BOOLEAN;
PROCEDURE MustBe(p : RecursiveParser.T; VAR t : T) RAISES { E };
PROCEDURE MustGet(p : RecursiveParser.T) : T RAISES { E };

CONST Brand = "DefIdent";

END DefIdent.
