MODULE CspDeclaration;
IMPORT CspDeclarator;
IMPORT CspDeclaratorSeq;
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

BEGIN END CspDeclaration.
