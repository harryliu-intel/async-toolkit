INTERFACE ModelVar;
IMPORT SchemeSymbol;
IMPORT StatComponent;
IMPORT ResponseModel;

TYPE
  T = RECORD
    nm      : SchemeSymbol.T;
    orders  : ARRAY StatComponent.T OF ResponseModel.Order;
  END;

PROCEDURE Format(READONLY t : T) : TEXT;
  
CONST Brand = "ModelVar";

END ModelVar.
