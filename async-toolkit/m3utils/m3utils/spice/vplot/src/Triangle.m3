MODULE Triangle;
IMPORT CardArraySort, Word;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN
    RETURN Sort(a.s) = Sort(b.s)
  END Equal;

PROCEDURE Hash(READONLY a : T) : Word.T =
  VAR
    res : Word.T := 0;
  BEGIN
    WITH sort = Sort(a.s) DO
      FOR i := FIRST(sort) TO LAST(sort) DO
        res := Word.Plus(res,Word.Plus(i,sort[i]))
      END
    END;
    RETURN res
  END Hash;

PROCEDURE Sort(READONLY r : ARRAY [0..2] OF CARDINAL) : 
                            ARRAY [0..2] OF CARDINAL =
  VAR
    q := r;
  BEGIN
    CardArraySort.Sort(q);
    RETURN q
  END Sort;

BEGIN END Triangle.
