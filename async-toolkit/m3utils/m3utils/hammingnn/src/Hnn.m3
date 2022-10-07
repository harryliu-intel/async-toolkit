MODULE Hnn;

IMPORT HnnSettings, HnnPrivate;
FROM HnnPrivate IMPORT RepIterator;

IMPORT HnnHrep;
IMPORT HnnHrepSeq;
IMPORT HnnHrepRefSet;
IMPORT HnnHrepRef;
IMPORT CardSeq;

REVEAL
  T = HnnPrivate.Private BRANDED Brand OBJECT
    seq : HnnHrepSeq.T;

    set : HnnHrepRefSet.T;
    (* store in a set to make sure we don't duplicate entries *)

    valid := FALSE;
    (* when the hash tables are filled in, this is set to TRUE *)

    len : CARDINAL;
    (* length of words stored *)
    
    s : [ 0..HnnSettings.MaxS ] := 0;
    (* 0 means automatic *)

    hashTabs : REF ARRAY OF ARRAY OF CardSeq.T;
    (* hashTabs is in size:
       CEILING(len/s)    x    2^s    x (sequences)
       ^                      ^
       pos in word            bin val of substr

       cardseq is index into t.seq // see above
    *)
       
    
  METHODS
    rehash() := Rehash;
  OVERRIDES

    (* Public methods *)
    init := Init;
    put := Put;
    iterClose := IterClose;
    iterNnOrdered := IterNnOrdered;
    iter := Iter;
    get := Get;
    size := Size;

    (* Settings methods *)
    setS := SetS;

    (* Private methods *)
    putRep := PutRep;
    iterCloseRep := IterCloseRep;
    iterNnOrderedRep := IterNnOrderedRep;
  END;

PROCEDURE Rehash(t : T) =
  BEGIN
  END Rehash;

PROCEDURE Init(t : T; len : CARDINAL) : T =
  BEGIN
  END Init;

PROCEDURE Put(t : T; READONLY elem : Elem) : CARDINAL =
  BEGIN END Put;
    
PROCEDURE IterClose(t : T;
                    READONLY elem : Elem; maxHamming : CARDINAL) : Iterator =
  BEGIN END IterClose;
  
PROCEDURE IterNnOrdered(t : T;
                        READONLY elem : Elem; n : CARDINAL) : Iterator =
  BEGIN END IterNnOrdered;
  
PROCEDURE Iter(t : T;
               ) : Iterator =
  BEGIN END Iter;
  
PROCEDURE Get(t : T;
              i : CARDINAL) =
  BEGIN END Get;
  
PROCEDURE Size(t : T;
               ) : CARDINAL =
  BEGIN END Size;

PROCEDURE SetS(t : T;
               s : [ 1 .. HnnSettings.MaxS ]) =
  BEGIN END SetS;
  
PROCEDURE PutRep(t : T;
                 READONLY elem : HnnHrep.T) =
  BEGIN END PutRep;
  
PROCEDURE IterCloseRep(t : T;
                       READONLY elem : HnnHrep.T; maxHamming : CARDINAL) : RepIterator =
  BEGIN END IterCloseRep;

PROCEDURE IterNnOrderedRep(t : T;
                           READONLY elem : HnnHrep.T; n : CARDINAL) : RepIterator =
  BEGIN END IterNnOrderedRep;
    





  BEGIN END Hnn.
