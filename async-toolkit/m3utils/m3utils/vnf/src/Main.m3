(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE Main;

IMPORT vnfParseStd, vnfLexStd;
IMPORT Stdio;

VAR
  lexer := NEW(vnfLexStd.T);
  parser := NEW(vnfParseStd.T);
BEGIN
  EVAL lexer.setRd(Stdio.stdin);
  EVAL parser.setLex(lexer);

  (* first parse input *)
  EVAL parser.parse(); 
END Main.
