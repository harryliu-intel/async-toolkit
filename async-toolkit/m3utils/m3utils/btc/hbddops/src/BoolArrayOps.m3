(* $Id$ *)

MODULE BoolArrayOps;

PROCEDURE ColsEq(READONLY a : ARRAY OF ARRAY OF BOOLEAN;
                 p, q : CARDINAL) : BOOLEAN =
  BEGIN
    FOR row := FIRST(a) TO LAST(a) DO
      IF a[row,p] # a[row,q] THEN 
        RETURN FALSE
      END
    END;
    RETURN TRUE
  END ColsEq;

PROCEDURE MaxTwoDiffCols(READONLY a : ARRAY OF ARRAY OF BOOLEAN) : BOOLEAN =

  VAR
    rp1 := -1;
  BEGIN
    FOR col := FIRST(a[0]) TO LAST(a[0]) DO
      IF NOT ColsEq(a, 0, col) THEN
        IF rp1 = -1 THEN 
          rp1 := col 
        ELSIF NOT ColsEq(a, rp1, col) THEN
          RETURN FALSE
        END
      END
    END;
    RETURN TRUE
  END MaxTwoDiffCols;


BEGIN END BoolArrayOps.
