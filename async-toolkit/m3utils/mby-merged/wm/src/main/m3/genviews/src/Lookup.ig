(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

GENERIC INTERFACE Lookup(Type);
IMPORT ParseError;

PROCEDURE Parse(str : TEXT) : Type.T RAISES { ParseError.E };

CONST Brand = "Lookup(" & Type.Brand & ")";

END Lookup.
