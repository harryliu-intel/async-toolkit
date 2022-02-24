INTERFACE LibertyHead;
IMPORT LibertyComponent;
IMPORT LibertyParamList;

TYPE
  T <: Public;

  Public = LibertyComponent.T OBJECT
    ident  : TEXT;
    params : LibertyParamList.T;
  END;

PROCEDURE New(ident : TEXT; params : LibertyParamList.T) : T;

CONST Brand = "LibertyHead";

END LibertyHead.
