(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Glitch;
IMPORT GlitchExpr;

PROCEDURE Output(nm : TEXT);

PROCEDURE Async(nm : TEXT);

PROCEDURE Gate(tgt : TEXT; expr : GlitchExpr.T);

PROCEDURE Literal(nm : TEXT; expr : GlitchExpr.T);

PROCEDURE GetLiteral(nm : TEXT) : GlitchExpr.T;

PROCEDURE RunChecks(asyncLimit, alg1MaxDepth : CARDINAL) : BOOLEAN
  RAISES { Timeout } ;
  
CONST Brand = "Glitch";

EXCEPTION Timeout;
      
END Glitch.
