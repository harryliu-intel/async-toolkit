INTERFACE LibertyParamList;
IMPORT LibertyAttrValSeq;
IMPORT LibertyComponent;

(* this type is not exposed to the user anymore, it's just used as a vessel
   to get the components into LibertyHead.T *)

TYPE
  T <: Public;

  Public = LibertyComponent.T OBJECT
    sep    : TEXT;
    params : LibertyAttrValSeq.T;
  END;

CONST Brand = "LibertyParamList";

PROCEDURE New() : T; (* return an empty but initialized list *)
      
END LibertyParamList.
