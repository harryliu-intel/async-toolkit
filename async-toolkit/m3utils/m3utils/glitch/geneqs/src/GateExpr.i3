(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE GateExpr;
IMPORT TextSet;

TYPE
  T = OBJECT nm : TEXT END;

  Named = T BRANDED OBJECT
  END;

  Expr = T BRANDED OBJECT
    op : Op;
    a, b : T;
  END;

  Op = { And, Or, Not, Xor };

PROCEDURE And(a, b : T) : T;
PROCEDURE Or(a, b : T) : T;
PROCEDURE Xor(a, b : T) : T;
PROCEDURE Not(a : T) : T;
PROCEDURE New(nm : TEXT) : T;

CONST Brand = "GateExpr";

PROCEDURE Format(a : T; not := "!") : TEXT;

PROCEDURE Fanins(a : T) : TextSet.T;
  
END GateExpr.
