INTERFACE Histogram;
IMPORT OSError, Wr, Thread;

CONST Brand = "Histogram";

PROCEDURE Do(ofn          : TEXT;
             
             READONLY res : ARRAY OF LONGREAL;
             (* must be sorted *)

             low          : BOOLEAN;
             (* denotes whether desired value is low or high, for the
                yield loss chart *)

             H            : CARDINAL := DefaultBuckets (* # of buckets *);

             G            : CARDINAL := DefaultLossSteps
  ) RAISES { OSError.E, Wr.Failure, Thread.Alerted };

CONST DefaultBuckets   = 15;
      DefaultLossSteps = 1000;

END Histogram.
