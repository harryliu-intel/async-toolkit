(* $Id: Subarrays.mg,v 1.1 2006/03/19 00:10:06 mika Exp $ *)

GENERIC MODULE Subarrays(Elem);

REVEAL
  Iterator = PublicIterator BRANDED Brand & " Iterator" OBJECT
    arr : REF ARRAY OF Elem.T;
    cur : RarrC;
   OVERRIDES
    next := INext;
  END;

PROCEDURE IterateOfSize(arr : REF ARRAY OF Elem.T; 
                        size : CARDINAL) : Iterator =
  VAR
    res := NEW(Iterator, arr := arr,
               cur := NEW(RarrC, size));
  BEGIN
    FOR i := FIRST(res.cur^) TO LAST(res.cur^) DO
      res.cur[i] := i
    END;
    RETURN res
  END IterateOfSize;

PROCEDURE INext(iter : Iterator; VAR iarr : ARRAY OF Elem.T) : BOOLEAN =
  BEGIN
    IF iter.cur = NIL THEN RETURN FALSE END;

    FOR i := FIRST(iarr) TO LAST(iarr) DO
      iarr[i] := iter.arr[iter.cur[i]]
    END;

    (* increment *)
    FOR i := FIRST(iarr) TO LAST(iarr) DO
      WITH c = iter.cur^, max = NUMBER(iter.arr^)-1-(LAST(iarr)-i) DO
        IF c[i] < max AND (i=LAST(c) OR c[i] < c[i+1]-1) THEN
          INC(c[i]);
          RETURN TRUE
        ELSE
          (* reset it *)
          c[i] := i
        END
      END
    END;

    iter.cur := NIL; (* this was the last! *)
    RETURN TRUE
  END INext;

BEGIN END Subarrays.
