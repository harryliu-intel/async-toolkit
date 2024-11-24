INTERFACE ModelVar;
IMPORT SchemeSymbol;
IMPORT QuadResponse;
IMPORT ResponseModel;

TYPE
  T = RECORD
    nm      : SchemeSymbol.T;
    models  : ARRAY QuadResponse.T OF ResponseModel.Order;
  END;

PROCEDURE Format(READONLY t : T) : TEXT;
  
CONST Brand = "ModelVar";

END ModelVar.
