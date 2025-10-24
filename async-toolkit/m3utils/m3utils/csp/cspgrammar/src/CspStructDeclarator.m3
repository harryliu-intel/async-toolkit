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
