GENERIC MODULE Interval(Float);

PROCEDURE Size(i: T): Float.T =
  BEGIN
    RETURN i.hi - i.lo;
  END Size;

BEGIN
END Interval.
