INTERFACE BnfClass;
IMPORT Bnf;

REVEAL
  Bnf.T <: Private;
  
TYPE
  Private = Bnf.Public OBJECT METHODS
    copy() : Bnf.T;
    deepCopy() : Bnf.T;
  END;

CONST Brand = "BnfClass";

END BnfClass.
  
