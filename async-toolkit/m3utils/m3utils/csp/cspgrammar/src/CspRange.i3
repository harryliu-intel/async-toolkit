(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE CspRange;
IMPORT CspExpression;
IMPORT SchemePair;

TYPE
  T = RECORD
    min, max : CspExpression.T; (* may not be NIL *)
  END;

CONST Brand = "CspRange";

PROCEDURE Lisp(READONLY t : T) : SchemePair.T;
  
END CspRange.
