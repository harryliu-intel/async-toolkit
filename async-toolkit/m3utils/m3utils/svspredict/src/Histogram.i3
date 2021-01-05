INTERFACE Histogram;
IMPORT OSError, Wr, Thread;

CONST Brand = "Histogram";

PROCEDURE Do(ofn          : TEXT;
             
             READONLY res : ARRAY OF LONGREAL;
             (* must be sorted *)

             low          : BOOLEAN;
             (* denotes whether desired value is low or high *)

             H            : CARDINAL := DefaultBuckets (* # of buckets *)
  ) RAISES { OSError.E, Wr.Failure, Thread.Alerted };

CONST DefaultBuckets = 15;

END Histogram.
