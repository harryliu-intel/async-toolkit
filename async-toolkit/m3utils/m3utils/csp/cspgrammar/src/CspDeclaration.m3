(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE CspDeclaration;
IMPORT CspDeclarator;
IMPORT CspDeclaratorSeq;
IMPORT CspStructDeclarator;
IMPORT CspStructDeclaratorSeq;
IMPORT SchemePair;
FROM SchemeUtils IMPORT Cons;

PROCEDURE CspDeclaratorSeqLisp(seq : CspDeclaratorSeq.T) : SchemePair.T =
  VAR
    p : SchemePair.T := NIL;
  BEGIN
    FOR i := seq.size() - 1 TO 0 BY -1 DO
      p := Cons(CspDeclarator.Lisp(seq.get(i)), p)
    END;
    RETURN p
  END CspDeclaratorSeqLisp;

PROCEDURE CspStructDeclaratorSeqLisp(seq : CspStructDeclaratorSeq.T) : SchemePair.T =
  VAR
    p : SchemePair.T := NIL;
  BEGIN
    FOR i := seq.size() - 1 TO 0 BY -1 DO
      p := Cons(CspStructDeclarator.Lisp(seq.get(i)), p)
    END;
    RETURN p
  END CspStructDeclaratorSeqLisp;

BEGIN END CspDeclaration.
