(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE GlitchExprEval;
IMPORT TextGlitchExprTbl;
IMPORT GlitchExpr;
IMPORT Text01XTbl;
IMPORT ZeroOneX;

PROCEDURE Eval(x        : GlitchExpr.T;
               literals : Text01XTbl.T;
               gates    : TextGlitchExprTbl.T) : ZeroOneX.T;
  
END GlitchExprEval.
