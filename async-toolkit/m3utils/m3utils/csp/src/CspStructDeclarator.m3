MODULE CspStructDeclarator;
IMPORT SchemePair;
IMPORT SchemeSymbol;
FROM SchemeUtils IMPORT Cons;
IMPORT CspSyntax;
IMPORT CspDeclarator;

CONST Sym = SchemeSymbol.FromText;

PROCEDURE Lisp(READONLY t : T) : SchemePair.T =
  VAR
    declLisp := CspDeclarator.Lisp(t.decl);
  BEGIN
    declLisp.first := CspSyntax.Lisp(t.init);
    RETURN Cons(Sym("structdecl"),
                    declLisp)
  END Lisp;

BEGIN END CspStructDeclarator.
