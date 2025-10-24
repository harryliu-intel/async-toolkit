(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE CspStructDeclarator;
IMPORT CspDeclarator;
IMPORT CspExpression;
IMPORT SchemePair;

(* like a CspDeclarator but with an initial value *)

TYPE
  T = RECORD
    init : CspExpression.T;
    decl : CspDeclarator.T;
  END;

CONST Brand = "CspStructDeclarator";

PROCEDURE Lisp(READONLY t : T) : SchemePair.T;
  
END CspStructDeclarator.
