(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE GlitchGate;
IMPORT GlitchExpr;
IMPORT BDD;

TYPE
  T = OBJECT
    tgt    : TEXT;
    tgtBdd : BDD.T;
    expr   : GlitchExpr.T;
  END;

PROCEDURE Equal(a, b : T) : BOOLEAN;

CONST Brand = "GlitchGate";

END GlitchGate.
