(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE CspSyntax;
IMPORT SchemeObject;

TYPE
  T = Public BRANDED Brand OBJECT END;

  Public = OBJECT METHODS
    lisp() : SchemeObject.T;
  END;

CONST Brand = "CspSyntax";

PROCEDURE Lisp(of : T) : SchemeObject.T;
  (* utility function that passes NIL through *)

END CspSyntax.
    
