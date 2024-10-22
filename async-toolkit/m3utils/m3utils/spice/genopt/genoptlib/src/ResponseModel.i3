INTERFACE ResponseModel;
IMPORT SchemeSymbol;

TYPE
  Type = { Linear, Quadratic };

  T = RECORD
    symbol : SchemeSymbol.T;
    type   : Type;
  END;

CONST TypeNames = ARRAY Type OF TEXT { "Linear", "Quadratic" };

CONST Brand = "ResponseModel";

END ResponseModel.
    
