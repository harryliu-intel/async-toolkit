(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE CspStructDeclarator;
IMPORT SchemePair;
IMPORT SchemeSymbol;
FROM SchemeUtils IMPORT Cons, Reverse;
IMPORT CspSyntax;
IMPORT CspDeclarator;
IMPORT SchemeObject;

PROCEDURE Lisp(READONLY t : T) : SchemePair.T =
  VAR
    declLisp := CspDeclarator.Lisp(t.decl);
    initLisp := CspSyntax.Lisp(t.init);
  BEGIN
    RETURN Reverse(Cons(initLisp,
                        Reverse(declLisp)))
  END Lisp;

BEGIN END CspStructDeclarator.
