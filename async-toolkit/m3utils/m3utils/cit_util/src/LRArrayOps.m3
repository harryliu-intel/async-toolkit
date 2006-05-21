(* $Id$ *)

MODULE LRArrayOps;
IMPORT LongrealArraySort;

PROCEDURE Percentile(READONLY a : ARRAY OF LONGREAL; p : LONGREAL) : LONGREAL =
  (* a must be sorted *)
  VAR
    q := p * FLOAT(LAST(a),LONGREAL);
    n := TRUNC(q);
    np1 := n+1;
    w1 := q - FLOAT(n,LONGREAL);
    w0 := 1.0d0 - w1;
    res : LONGREAL;
  CONST
    EPS = 1.0d-6;
  BEGIN
    IF np1 <= LAST(a) THEN
      res := w0 * a[n] + w1 * a[np1];
    ELSE
      <* ASSERT n = LAST(a) *>
      <* ASSERT ABS(w1) < EPS *>
      res := a[n]
    END;   
    RETURN res   
  END Percentile;

PROCEDURE Sort(VAR a : ARRAY OF LONGREAL) =
  BEGIN LongrealArraySort.Sort(a) END Sort;

BEGIN END LRArrayOps.
