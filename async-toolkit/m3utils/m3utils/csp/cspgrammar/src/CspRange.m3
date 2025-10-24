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
