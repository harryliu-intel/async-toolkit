INTERFACE LibertyParamList;
IMPORT LibertyAttrValSeq;
IMPORT LibertyComponent;

TYPE
  T <: Public;

  Public = LibertyComponent.T OBJECT
    sep    : TEXT;
    params : LibertyAttrValSeq.T;
  END;

CONST Brand = "LibertyParamList";

PROCEDURE New() : T; (* return an empty but initialized list *)
      
END LibertyParamList.
