INTERFACE FingerprintFinder;
IMPORT Pathname, OSError, Rd;

PROCEDURE Find(pn : Pathname.T; key := "FINGERPRINT") : TEXT
  RAISES { OSError.E, Rd.Failure };
  (* find a line keyed FINGERPRINT in a file
     returns NIL if no such line or if it is malformed (whatever that means)  *)

END FingerprintFinder.
  
