INTERFACE SimModel;
IMPORT Valenv;

TYPE
  T = OBJECT METHODS
    simStep(e : Valenv.T);
  END;

CONST Brand = "SimModel";

END SimModel.
