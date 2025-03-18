MODULE CspDeclarator;
IMPORT SchemePair;
FROM SchemeUtils IMPORT List5;
IMPORT CspSyntax;
IMPORT SchemeSymbol;
IMPORT CspDirection;

CONST Sym = SchemeSymbol.FromText;

PROCEDURE Lisp(READONLY t : T) : SchemePair.T =
  BEGIN
    RETURN List5(Sym("decl"),
                 t.ident,
                 CspSyntax.Lisp(t.typeFragment),
                 CspSyntax.Lisp(t.init),
                 Sym(CspDirection.Names[t.direction]))
  END Lisp;

BEGIN END CspDeclarator.
