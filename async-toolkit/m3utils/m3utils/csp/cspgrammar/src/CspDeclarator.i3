(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE CspDeclarator;
IMPORT CspDirection;
IMPORT Atom;
IMPORT CspType;
IMPORT SchemePair;

TYPE
  T = RECORD
    ident        : Atom.T;
    typeFragment : CspType.T;
    direction    : CspDirection.T;
  END;

CONST Brand = "CspDeclarator";

PROCEDURE Lisp(READONLY t : T) : SchemePair.T;
  
END CspDeclarator.
