MODULE Hnn;

IMPORT HnnSettings, HnnPrivate;
FROM HnnPrivate IMPORT RepIterator;

IMPORT HnnHrep;
IMPORT HnnHrepSeq;
IMPORT HnnHrepCardTbl;
IMPORT CardSeq;

REVEAL
  T = HnnPrivate.Private BRANDED Brand OBJECT
    seq : HnnHrepSeq.T;

    set : HnnHrepCardTbl.T;
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
    init             := Init;
    put              := Put;
    iterClose        := IterClose;
    iterNnOrdered    := IterNnOrdered;
    iter             := Iter;
    get              := Get;
    size             := Size;

    (* Settings methods *)
    setS             := SetS;

    (* Private methods *)
    putRep           := PutRep;
    iterCloseRep     := IterCloseRep;
    iterNnOrderedRep := IterNnOrderedRep;
  END;

PROCEDURE Rehash(t : T) =
  BEGIN
    (* we should really be able to pick s automatically... *)
    <*ASSERT t.s # 0*>

    
  END Rehash;

PROCEDURE Init(t : T; len : CARDINAL) : T =
  BEGIN
    t.seq := NEW(HnnHrepSeq.T).init();
    t.set := NEW(HnnHrepCardTbl.Default).init();
    t.valid := FALSE;
    t.len := len;
    t.s := 0;
    t.hashTabs := NIL;
    RETURN t
  END Init;

PROCEDURE Put(t : T; READONLY elem : Elem) : CARDINAL =
  BEGIN
    WITH rep = HnnHrep.New(elem) DO
      RETURN t.putRep(rep)
    END
  END Put;
    
PROCEDURE IterClose(t : T;
                    READONLY elem : Elem; maxHamming : CARDINAL) : Iterator =
  BEGIN
  END IterClose;
  
PROCEDURE IterNnOrdered(t             : T;
                        READONLY elem : Elem;
                        n             : CARDINAL;
                        maxHamming    : CARDINAL) : Iterator =
  BEGIN
  END IterNnOrdered;
  
PROCEDURE Iter(t : T;
               ) : Iterator =
  BEGIN
  END Iter;
  
PROCEDURE Get(t : T;
              i : CARDINAL) =
  BEGIN
  END Get;
  
PROCEDURE Size(t : T;
               ) : CARDINAL =
  BEGIN
  END Size;

PROCEDURE SetS(t : T;
               s : [ 1 .. HnnSettings.MaxS ]) =
  BEGIN
  END SetS;
  
PROCEDURE PutRep(t : T;
                 elem : HnnHrep.T) : CARDINAL =
  VAR
    idx : CARDINAL;
  BEGIN
    (* check if we already have it, if we do, just return the index *)
    IF t.set.get(elem, idx) THEN
      RETURN idx
    ELSE
      idx := t.set.size();
    END;
    
    (* clear out hash tables if they exist *)
    t.valid := FALSE;
    t.hashTabs := NIL;

    (* make Rep and store it *)
    elem.id := idx;
    EVAL t.set.put(elem, idx);

    (* dont need to do more right now, just return the index *)
    RETURN idx

  END PutRep;
  
PROCEDURE IterCloseRep(t : T;
                       elem : HnnHrep.T; maxHamming : CARDINAL) : RepIterator =
  BEGIN
  END IterCloseRep;

PROCEDURE IterNnOrderedRep(t             : T;
                           elem          : HnnHrep.T;
                           n             : CARDINAL;
                           maxHamming    : CARDINAL) : RepIterator =
  BEGIN
  END IterNnOrderedRep;
    





BEGIN END Hnn.
