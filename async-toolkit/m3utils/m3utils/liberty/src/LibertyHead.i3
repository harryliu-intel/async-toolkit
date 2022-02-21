INTERFACE LibertyHead;
IMPORT LibertyComponent;
IMPORT LibertyParamList;

TYPE
  T <: LibertyComponent.T;

PROCEDURE New(ident : TEXT; params : LibertyParamList.T) : T;

CONST Brand = "LibertyHead";

END LibertyHead.
