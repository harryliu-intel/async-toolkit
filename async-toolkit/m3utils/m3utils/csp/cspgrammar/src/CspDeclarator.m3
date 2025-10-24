(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE CspDeclarator;
IMPORT SchemePair;
FROM SchemeUtils IMPORT List2, List4;
IMPORT CspSyntax;
IMPORT SchemeSymbol;
IMPORT CspDirection;

CONST Sym = SchemeSymbol.FromText;

PROCEDURE Lisp(READONLY t : T) : SchemePair.T =
  BEGIN
    RETURN List4(Sym("decl1"),
                 List2(Sym("id"), t.ident),
                 CspSyntax.Lisp(t.typeFragment),
                 Sym(CspDirection.Names[t.direction])
    )
  END Lisp;

BEGIN END CspDeclarator.
