(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE RegFieldAccess;
IMPORT Text;
IMPORT ParseError;

CONST TE = Text.Equal;
      
PROCEDURE Parse(nm : TEXT) : OpSet RAISES { ParseError.E } =
  BEGIN
    FOR i := FIRST(Named) TO LAST(Named) DO
      IF TE(nm, Named[i].nm) THEN RETURN Named[i].opSet END
    END;
    RAISE ParseError.E(nm)
  END Parse;

BEGIN END RegFieldAccess.
