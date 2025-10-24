(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE LibertySimpleAttr;
IMPORT LibertyComponent;
IMPORT LibertyAttrValExpr;

TYPE
  T <: Public;

  Public = LibertyComponent.T OBJECT
    ident       : TEXT;
    attrValExpr : LibertyAttrValExpr.T;
    syntax      : Syntax;
  END;

  Syntax = { ColonSemi, Colon, Eq };
  
CONST Brand = "LibertySimpleAttr";

END LibertySimpleAttr.
 
