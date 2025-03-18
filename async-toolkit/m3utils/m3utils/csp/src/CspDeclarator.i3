INTERFACE CspDeclarator;
IMPORT CspDirection;
IMPORT Atom;
IMPORT CspExpression;
IMPORT CspType;
IMPORT SchemePair;

TYPE
  T = RECORD
    ident        : Atom.T;
    typeFragment : CspType.T;
    init         : CspExpression.T;
    direction    : CspDirection.T;
  END;

CONST Brand = "CspDeclarator";

PROCEDURE Lisp(READONLY t : T) : SchemePair.T;
  
END CspDeclarator.
