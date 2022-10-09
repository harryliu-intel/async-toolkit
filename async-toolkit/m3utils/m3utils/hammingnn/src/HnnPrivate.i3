INTERFACE HnnPrivate;
IMPORT Hnn;
IMPORT HnnSettings;
IMPORT HnnHrep;

REVEAL
  Hnn.T <: Private;

TYPE
  Private = HnnSettings.Settings OBJECT METHODS

    putRep(elem : HnnHrep.T) : CARDINAL;
    (* note rep exposure here:
       elem is modified and inserted into the data structure,
       do not modify elem subsequently!
    *)

    iterCloseRep(elem : HnnHrep.T; maxHamming : CARDINAL) : RepIterator;
    
    iterNnOrderedRep(elem : HnnHrep.T;
                     n : CARDINAL;
                     maxHamming : CARDINAL := 0) : RepIterator;

    getRep(i : CARDINAL; VAR elem : HnnHrep.T);

    iterRep() : RepIterator;
    
  END;

  RepIterator = OBJECT METHODS
    next(VAR e : HnnHrep.T) : BOOLEAN;
  END;
  
END HnnPrivate.
