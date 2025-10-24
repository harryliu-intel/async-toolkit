INTERFACE FingerprintFinder;
IMPORT Pathname;

CONST DefKey = "FINGERPRINT";
      
PROCEDURE Find(pn : Pathname.T; key := DefKey) : TEXT;
  (* find a line keyed FINGERPRINT in a file

     returns NIL if :

     -- no such file
     -- no such line or 
     -- if it is malformed (whatever that means)  
  *)

END FingerprintFinder.
  
