INTERFACE ResponseModel;
IMPORT SchemeSymbol;

TYPE
  Type = { Linear, Quadratic };

  T = RECORD
    symbol : SchemeSymbol.T;
    type   : Type;
  END;

CONST Brand = "ResponseModel";

END ResponseModel.
    
