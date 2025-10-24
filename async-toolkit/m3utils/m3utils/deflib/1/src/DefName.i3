(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE DefName;
IMPORT RecursiveParser;
FROM ParseError IMPORT E;

TYPE T = RECORD END; (* TBD *)

PROCEDURE Get(t : RecursiveParser.T; VAR name : T) : BOOLEAN RAISES { E };
PROCEDURE MustBe(t : RecursiveParser.T; VAR name : T) RAISES { E };
PROCEDURE MustGet(t : RecursiveParser.T) : T RAISES { E };

CONST Brand = "DefName";

END DefName.
