(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

(* $Id$ *)

MODULE PGSQLScan;
IMPORT Text, Lex;

PROCEDURE Bool(txt : TEXT) : BOOLEAN RAISES { Lex.Error } =
  BEGIN
    IF    Text.Equal(txt, "t") THEN RETURN TRUE
    ELSIF Text.Equal(txt, "f") THEN RETURN FALSE
    ELSE 
      RAISE Lex.Error
    END
  END Bool;

BEGIN END PGSQLScan.
