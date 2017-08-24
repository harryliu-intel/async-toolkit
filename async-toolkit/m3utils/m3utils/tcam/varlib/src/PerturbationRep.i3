INTERFACE PerturbationRep;
IMPORT Perturbation;
IMPORT TextLongRealTbl AS TextLRTbl;

REVEAL
  Perturbation.Default <: Private;
  
TYPE
  Private = Perturbation.PubDefault OBJECT
    model, var  : TEXT;
  END;
  
END PerturbationRep.
