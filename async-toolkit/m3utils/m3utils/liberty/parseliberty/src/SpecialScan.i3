(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE SpecialScan;
IMPORT Lex, FloatMode;

PROCEDURE Int(txt: TEXT; defaultBase: [2..16] := 10): INTEGER
  RAISES {Lex.Error, FloatMode.Trap};

PROCEDURE LongReal(txt: TEXT): LONGREAL
  RAISES {Lex.Error, FloatMode.Trap};

PROCEDURE String(txt : TEXT) : TEXT;
  
END SpecialScan.
