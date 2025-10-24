(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE WebAppUtils;
IMPORT TextTable, FloatMode, Lex;

CONST VerboseDebug = TRUE;

PROCEDURE ParseQuestionMarkData(data : TEXT;
                                debug := VerboseDebug) : TextTable.T 
  RAISES { FloatMode.Trap, Lex.Error };

PROCEDURE ParseEnv(verboseDebug := VerboseDebug) : TextTable.T;

CONST Brand = "WebAppUtils";

END WebAppUtils.
