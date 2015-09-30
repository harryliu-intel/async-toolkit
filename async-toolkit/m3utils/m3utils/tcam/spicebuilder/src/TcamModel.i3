INTERFACE TcamModel;
IMPORT Tcam;
IMPORT SimModel;

TYPE
  T <: Public;

  Public = SimModel.T OBJECT METHODS
    init(READONLY c : Tcam.T) : T;
  END;

CONST Brand = "TcamModel";

END TcamModel.
    
