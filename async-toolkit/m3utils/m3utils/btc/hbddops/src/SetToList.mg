(* $Id$ *)

GENERIC MODULE SetToList(Base, BaseSet);
IMPORT SchemePair;

PROCEDURE SetToList(set : BaseSet.T) : SchemePair.T =
  VAR res : SchemePair.T := NIL;
      b : Base.T;
  BEGIN
    WITH iter = set.iterate() DO
      WHILE iter.next(b) DO
        res := NEW(SchemePair.T, first := b, rest := res)
      END
    END;
    RETURN res
  END SetToList;

PROCEDURE SetToArr(s : BaseSet.T) : REF ARRAY OF Base.T =
  VAR
    sz   := s.size();
    arr  := NEW(REF ARRAY OF Base.T, sz);
  BEGIN
    WITH iter = s.iterate() DO
      FOR i := 0 TO sz-1 DO EVAL iter.next(arr[i]) END
    END;
    RETURN arr
  END SetToArr;

BEGIN END SetToList.

