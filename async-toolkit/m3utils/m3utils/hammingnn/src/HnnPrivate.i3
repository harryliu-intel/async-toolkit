INTERFACE HnnPrivate;
IMPORT Hnn;
IMPORT HnnSettings;
IMPORT HnnHrep;

REVEAL
  Hnn.T <: Private;

TYPE
  Private = HnnSettings.Settings OBJECT METHODS
    putRep(READONLY elem : HnnHrep.T);
    iterCloseRep(READONLY elem : HnnHrep.T; maxHamming : CARDINAL) : RepIterator;
    iterNnOrderedRep(READONLY elem : HnnHrep.T; n : CARDINAL) : RepIterator;
  END;

  RepIterator = OBJECT METHODS
    next(VAR e : HnnHrep.T) : BOOLEAN;
  END;
  
END HnnPrivate.
