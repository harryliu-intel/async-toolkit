INTERFACE LambModel;
IMPORT Lamb;
IMPORT SimModel;

TYPE
  T <: Public;

  Public = SimModel.T OBJECT METHODS
    init(READONLY c : Lamb.T;
         cycleTime, assertHoldFrac, assertHoldTime : LONGREAL) : T;

  END;

CONST Brand = "LambModel";

END LambModel.
    
