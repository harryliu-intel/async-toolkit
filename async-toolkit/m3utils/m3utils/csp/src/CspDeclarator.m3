MODULE CspDeclarator;
IMPORT SchemePair;
FROM SchemeUtils IMPORT List2, List5;
IMPORT CspSyntax;
IMPORT SchemeSymbol;
IMPORT CspDirection;

CONST Sym = SchemeSymbol.FromText;

PROCEDURE Lisp(READONLY t : T) : SchemePair.T =
  BEGIN
    RETURN List5(Sym("decl"),
                 List2(Sym("id"), t.ident),
                 CspSyntax.Lisp(t.typeFragment),
                 Sym(CspDirection.Names[t.direction]),
                 CspSyntax.Lisp(t.init)
    )
  END Lisp;

BEGIN END CspDeclarator.
