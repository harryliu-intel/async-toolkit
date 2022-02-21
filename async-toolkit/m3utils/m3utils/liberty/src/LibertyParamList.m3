MODULE LibertyParamList;
IMPORT LibertyAttrValSeq;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    format := Format;
  END;

PROCEDURE Format(t : T) : TEXT =
  BEGIN
  END Format;

PROCEDURE New() : T =
  BEGIN
    RETURN NEW(T,
               sep    := NIL,
               params := NEW(LibertyAttrValSeq.T).init())
  END New;
  
BEGIN END LibertyParamList.
