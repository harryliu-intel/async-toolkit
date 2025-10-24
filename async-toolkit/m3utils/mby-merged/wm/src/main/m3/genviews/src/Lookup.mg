(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

GENERIC MODULE Lookup(Type);
IMPORT Text;
IMPORT ParseError;
CONST TE = Text.Equal;
      
PROCEDURE Parse(str : TEXT) : Type.T RAISES { ParseError.E } =
  BEGIN
    FOR i := FIRST(Type.Map) TO LAST(Type.Map) DO
      IF TE(Type.Map[i].nm, str) THEN RETURN Type.Map[i] END
    END;
    RAISE ParseError.E("Not member of " & Type.Brand & " Map : " & str)
  END Parse;
  
BEGIN END Lookup.
