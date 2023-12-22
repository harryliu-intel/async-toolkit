MODULE LibertyParamList;
IMPORT LibertyComponentChildren;
IMPORT LibertyAttrValSeq;
IMPORT Wr;
IMPORT Thread;
IMPORT LibertyComponentSeq;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
  END;

PROCEDURE New() : T =
  BEGIN
    RETURN NEW(T,
               sep    := NIL,
               params := NEW(LibertyAttrValSeq.T).init())
  END New;

BEGIN END LibertyParamList.
