INTERFACE CspDeclaration;
IMPORT CspDeclaratorSeq;
IMPORT CspSyntax;
IMPORT SchemePair;

TYPE
  T = CspSyntax.T;

  Function <: T;

  Structure <: T;

CONST Brand = "CspDeclaration";

PROCEDURE CspDeclaratorSeqLisp(seq : CspDeclaratorSeq.T) : SchemePair.T;

END CspDeclaration.
