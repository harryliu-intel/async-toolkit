INTERFACE LibertyHead;
IMPORT LibertyComponent;
IMPORT LibertyParamList;
IMPORT LibertyAttrValSeq;

TYPE
  T <: Public;

  Public = LibertyComponent.T OBJECT
    ident  : TEXT;
    sep    : TEXT;
    params : LibertyAttrValSeq.T;
  END;

PROCEDURE New(ident : TEXT; params : LibertyParamList.T) : T;

CONST Brand = "LibertyHead";

END LibertyHead.
