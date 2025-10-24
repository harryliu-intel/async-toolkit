(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE LibertyAttrValExpr;
IMPORT LibertyComponent;
IMPORT LibertyBoolean;
IMPORT LibertyExpr;

TYPE
  T <: LibertyComponent.T;

  String = T OBJECT
    val : TEXT;
  END;

  Boolean = T OBJECT
    val : LibertyBoolean.T;
  END;

  Expr = T OBJECT
    val : LibertyExpr.T;
  END;

CONST Brand = "LibertyAttrValExpr";

END LibertyAttrValExpr.
