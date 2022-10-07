INTERFACE HnnHrep;
IMPORT Word;

TYPE
  T = RECORD
    (* s/b REF RECORD? do we ever need the non-REF version? *)

    bits        : REF ARRAY OF Word.T;
    (* the bits, in little-endian order 
       protected by hashValid!
    *)
    
    sz          : CARDINAL;
    (* may not need this *)
    
    hashV       : Word.T;
    hashValid := FALSE;
    (* hash value and valid bit for storage *)

    id          : CARDINAL; 
    (* id, s/n into seq in parent master table *)
    
  END;

PROCEDURE SetHash(VAR t : T);

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;
  (* checks a.bits^ = b.bits^, and sz
     ignores id
  *)

PROCEDURE Hash(VAR t : T) : Word.T;
  (* hash the bits (only), caches in hashV *)

CONST Brand = "HnnHrep";
  
END HnnHrep.
