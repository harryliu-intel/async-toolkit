INTERFACE CspDeclaration;
IMPORT CspExpression;
IMPORT CspDeclaratorSeq;
IMPORT Atom;

TYPE
  T = BRANDED Brand OBJECT
  END;

  Function <: T;

  Structure <: T;

CONST Brand = "CspDeclaration";

END CspDeclaration.
