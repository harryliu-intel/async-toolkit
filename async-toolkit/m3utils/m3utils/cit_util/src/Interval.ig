GENERIC INTERFACE Interval(Float);

TYPE
  T = RECORD
    lo, hi: Float.T;
  END;

PROCEDURE Size(i: T): Float.T;

END Interval.
