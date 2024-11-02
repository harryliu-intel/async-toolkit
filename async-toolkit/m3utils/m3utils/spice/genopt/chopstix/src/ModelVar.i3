INTERFACE ModelVar;
IMPORT SchemeSymbol;
IMPORT QuadResponse;
IMPORT ResponseModel;

TYPE
  T = RECORD
    nm      : SchemeSymbol.T;
    models  : ARRAY QuadResponse.T OF ResponseModel.Type;
  END;

PROCEDURE Format(READONLY t : T) : TEXT;
  
CONST Brand = "ModelVar";

END ModelVar.
