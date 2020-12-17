INTERFACE BnfVisit;
IMPORT Bnf;

TYPE
  T = OBJECT METHODS visit(t : Bnf.T)  END;

PROCEDURE Pre(t : Bnf.T; visitor : T);

PROCEDURE Post(t : Bnf.T; visitor : T);

CONST Brand = "BnfVisit";

END BnfVisit.
