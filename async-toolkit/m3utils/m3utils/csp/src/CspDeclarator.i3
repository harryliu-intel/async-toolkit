INTERFACE CspDeclarator;
IMPORT CspDirection;
IMPORT Atom;
IMPORT CspExpression;
IMPORT CspType;

TYPE
  T = RECORD
    ident        : Atom.T;
    typeFragment : CspType.T;
    init         : CspExpression.T;
    direction    : CspDirection.T;
  END;

CONST Brand = "CspDeclarator";

END CspDeclarator.
