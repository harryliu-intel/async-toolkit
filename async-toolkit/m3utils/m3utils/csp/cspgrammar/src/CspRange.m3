(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE CspRange;
IMPORT SchemePair;
IMPORT CspSyntax;
FROM SchemeUtils IMPORT List3;
IMPORT SchemeSymbol;
CONST Sym = SchemeSymbol.FromText;

PROCEDURE Lisp(READONLY t : T) : SchemePair.T =
  BEGIN
    RETURN List3(Sym("range"), CspSyntax.Lisp(t.min), CspSyntax.Lisp(t.max))
  END Lisp;

BEGIN END CspRange.
