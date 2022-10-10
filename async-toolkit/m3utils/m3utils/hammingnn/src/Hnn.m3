MODULE Hnn;

(* 
   Fast Search in Hamming Space with Multi-Index Hashing
   M. Norouzi et al

   2012 IEEE Conference on Computer Vision and Pattern Recognition 

   Author of this implementation : <mika.nystroem@intel.com>
   October, 2022
*)

IMPORT HnnSettings, HnnPrivate;
FROM HnnPrivate IMPORT RepIterator, PubRepIterator;

IMPORT HnnHrep;
IMPORT HnnHrepSeq;
IMPORT HnnHrepCardTbl;
IMPORT Word;
IMPORT CardSet, CardSetDef;
IMPORT CardPair;
IMPORT CardPairSeq;
IMPORT CardPairArraySort;
IMPORT Debug;
FROM Fmt IMPORT F, Int, Unsigned;

CONST doVerbose = FALSE;

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

    hashArr : REF ARRAY OF CARDINAL;
    (* hashArr holds the indices in the following format

           11 12 13 LC 6 7 22 LC LC
           ^           ^
    first bucket     second bucket  .. etc.

       LC = LAST(CARDINAL)
    *)
    
    
    hashTabs : HashTabs;
    (* hashTabs is in size:
       CEILING(len/s)    x    2^s    x (sequences)
       ^                      ^
       pos in word            bin val of substr

       cardseq is index into hashSeq // see above
    *)

    marked : CardSet.T;
    (* scratch pad for running matching *)
    
  METHODS
    rehash() := Rehash;
    nTabs() : CARDINAL := NTabs;
    nBucks() : CARDINAL := NBucks;
  OVERRIDES

    (* Public methods *)
    init             := Init;
    put              := Put;
    iterClose        := IterClose;
    iterNnOrdered    := IterNnOrdered;
    iter             := Iter;
    get              := Get;
    size             := Size;
    getLen           := GetLen;

    (* Settings methods *)
    setS             := SetS;

    (* Private methods *)
    putRep           := PutRep;
    getRep           := GetRep;
    iterCloseRep     := IterCloseRep;
    iterNnOrderedRep := IterNnOrderedRep;
    iterRep          := IterRep;
  END;

TYPE
  CardBase = CARDINAL;
  HashTabs = REF ARRAY OF ARRAY OF CardBase;

PROCEDURE NTabs(t : T) : CARDINAL =
  BEGIN
    RETURN (t.len - 1) DIV t.s + 1
  END NTabs;

PROCEDURE NBucks(t : T) : CARDINAL =
  BEGIN
    RETURN Word.Shift(1, t.s)      (* 2 ** t.s *) 
  END NBucks;
  
PROCEDURE NewHashTabs(tabls, bucks : CARDINAL) : HashTabs =
  VAR
    res : HashTabs;
  BEGIN
    IF doVerbose THEN
      Debug.Out(F("NewHashTabs: tabls=%s bucks=%s", Int(tabls), Int(bucks)))
    END;
    res := NEW(HashTabs, tabls, bucks);
    FOR i := FIRST(res^) TO LAST(res^) DO
      FOR j := FIRST(res[0]) TO LAST(res[0]) DO
        res[i, j] := 0
      END
    END;
    RETURN res
  END NewHashTabs;
  
PROCEDURE Rehash(t : T) =
  VAR
    r : HnnHrep.T;
    b : Word.T;
    hashPtr : HashTabs;
  BEGIN
    Debug.Out("Hnn.Rehash()");
    (* we should really be able to pick s automatically... *)
    IF t.s = 0 THEN
      t.s := MIN(t.len DIV 30,
                 HnnSettings.MaxS)
      (* optimize for about a billion entries *)
    END;

    WITH ntabls = t.nTabs(),
         nbucks = t.nBucks() DO

      t.hashTabs := NewHashTabs(ntabls, nbucks);
      (* this is the index of the beginning of each list *)

      hashPtr    := NewHashTabs(ntabls, nbucks);
      (* local, the pointer we use to write each bucket *)

      WITH hashArrN = t.seq.size() * ntabls + ntabls * nbucks DO
      (* the array holding the indices

         each number appears once in every table

         each table and each bucket has an end marker *)

        Debug.Out(F("Creating hash array: t.seq.size %s ntabls %s nbucks %s N %s",
                    Int(t.seq.size()),
                    Int(ntabls),
                    Int(nbucks),
                    Int(hashArrN)));
        
        t.hashArr  := NEW(REF ARRAY OF CARDINAL, hashArrN);
                        

        (* first mark each tab how big it needs to be *)
        FOR i := 0 TO t.seq.size() - 1 DO
          r := t.seq.get(i);
          FOR w := 0 TO ntabls - 1 DO
            b := HnnHrep.GetBits(r, w * t.s, t.s);
            INC(t.hashTabs[w, b])
          END
        END;

        Debug.Out("sizing hashTabs:");
        PrintFirstFew(t.hashTabs);
        
        (* accumulate hashTabs *)
        VAR
          p := 0;
        BEGIN
          FOR i := FIRST(t.hashTabs^) TO LAST(t.hashTabs^) DO
            FOR j := FIRST(t.hashTabs[0]) TO LAST(t.hashTabs[0]) DO
              VAR
                q := t.hashTabs[i,j] + 1;
                (* +1 leaves space for the tail marker *)
              BEGIN
                t.hashTabs[i,j] := p;
                p := p + q
              END
            END
          END;

          Debug.Out("pointerizing hashTabs:");
          PrintFirstFew(t.hashTabs);
        

          Debug.Out(F("Accumulated hashTabs sizes p %s hashArrN %s",
                      Int(p), Int(hashArrN)));
          <*ASSERT p = hashArrN*>

        END;
        
        (* at this point, hashTabs[.,.] contains the start index of each
           bucket in the hashArr *)
        
        (* make the pointers *)
        hashPtr^ := t.hashTabs^;
        
        (* now we store the ids in the hashArr *)
        FOR i := 0 TO t.seq.size() - 1 DO
          r := t.seq.get(i);
          FOR w := 0 TO ntabls - 1 DO
            b := HnnHrep.GetBits(r, w * t.s, t.s);
            t.hashArr[hashPtr[w, b]] := i;
            INC(hashPtr[w, b])
          END
        END;

        Debug.Out("wrote hashPtr:");
        PrintFirstFew(hashPtr);
        
        (* and write the tail element in each tab *)
        VAR
          p := 0; (* previous pointer, for validation *)
        BEGIN
          FOR i := FIRST(hashPtr^) TO LAST(hashPtr^) DO
            FOR j := FIRST(hashPtr[0]) TO LAST(hashPtr[0]) DO

              (* we can check that the previous pointer points to the
                   beginning of the current array *)
              IF t.hashTabs[i,j] # p THEN
                Debug.Error(F("hashTabs at %s # p %s ; i %s j %s",
                              Int(t.hashTabs[i,j]), Int(p), Int(i), Int(j)))
              END;
              
              WITH ptr = hashPtr[i,j] DO
                t.hashArr[ptr] := LAST(CARDINAL);
                INC(ptr);
                p := ptr
              END
            END
          END
        END(*RAV*);

        Debug.Out("completed hashPtr:");
        PrintFirstFew(hashPtr);

      END
    END;

    (* also initialize marked *)
    t.marked := NEW(CardSetDef.T).init();
    t.valid := TRUE

  END Rehash;

PROCEDURE PrintFirstFew(READONLY z : HashTabs) =
  BEGIN
    FOR i := 0 TO 2 DO
      FOR j := 0 TO 2 DO
        Debug.Out(F("z[%s , %s] = %s",
                    Int(i), Int(j), Int(z[i,j])))
      END
    END
  END PrintFirstFew;
  
PROCEDURE SeekMatches(w            : Word.T;
                      (* this is the word of which we seek matches *)

                      n            : CARDINAL;
                      (* this is the width of the data in the word *)
                      
                      maxDist      : CARDINAL;
                      (* this is the maximum Hamming distance of a match *)

                      READONLY tbl : ARRAY OF CardBase;
                      (* these are the hash buckets, 
                         containing indices into arr *)

                      READONLY arr : ARRAY OF CARDINAL;
                      
                      mark         : CardSet.T
                      (* if we find a match, we mark here *)
                      
                      ) =

  (* seek all the buckets that match w within a Hamming distance of maxDist

     e.g., if maxDist is 2 and w is 0000 then we search in the order

     0000 [preorder]
       1000
         1100
         1010
         1001
       0100
         0110
         0101
       0010
         0011
       0001

    # of dist 0 = 1
    # of dist 1 = 4
    # of dist 2 = C(4,2) = 4!/2!/2! = 3*4 / 2 = 6

    sum = 11

    for each word we mark a bit vector for every id that exists in that
    bucket
  *)
  

  PROCEDURE MarkBuckets(w : Word.T) =
    VAR
      p := tbl[w];
    BEGIN
      WHILE arr[p] # LAST(CARDINAL) DO
        WITH id  = arr[p] DO
          IF doVerbose THEN
            Debug.Out(F("MarkBuckets %s", Int(id)))
          END;
          EVAL mark.insert(id)
        END;
        INC(p)
      END
    END MarkBuckets;

  PROCEDURE Recurse(w        : Word.T;
                    maxDist  : CARDINAL;
                    startBit : CARDINAL) =
    BEGIN
      IF doVerbose THEN    
        Debug.Out(F("SeekMatches_Recurse: w=16_%s", Unsigned(w)))
      END;
      MarkBuckets(w);
      IF maxDist # 0 THEN
        FOR b := startBit TO MIN(startBit + maxDist, n) DO
          WITH ww = Word.Xor(w, Word.Shift(1, b)) DO
            Recurse(ww, maxDist - 1, startBit + 1)
          END
        END
      END
    END Recurse;
    
  BEGIN
    Recurse(w, maxDist, 0)
  END SeekMatches;

PROCEDURE SeekPotentialRepMatches(t                 : T;
                                  rep               : HnnHrep.T;
                                  maxBucketDistance : CARDINAL;
                                  marked            : CardSet.T) =
  (* find a global set of potential matches, by marking the mark array, 
     that match at least one bucket in at least maxBucketDistance bits *)
  BEGIN
    WITH ntabls = t.nTabs() DO
      FOR i := 0 TO ntabls - 1 DO
        WITH lob = i * t.s,                   (* first bit pos *)
             nxb = MIN(t.len - lob, t.s),     (* width of substring *)
             ww  = HnnHrep.GetBits(rep, lob, nxb)
         DO
          IF doVerbose THEN
            Debug.Out(F("SeekPotentialRepMatches: lob %s nxb %s ww 16_%s i %s",
                        Int(lob), Int(nxb), Unsigned(ww), Int(i)))
          END;
          SeekMatches(ww,
                      nxb,
                      maxBucketDistance,
                      t.hashTabs[i], t.hashArr^,
                      marked)
        END
      END
    END
  END SeekPotentialRepMatches;
                                  
PROCEDURE Init(t : T; len : CARDINAL) : T =
  BEGIN
    t.seq := NEW(HnnHrepSeq.T).init();
    t.set := NEW(HnnHrepCardTbl.Default).init();
    t.valid := FALSE;
    t.len := len;
    t.s := 0;
    t.hashTabs := NIL;
    t.hashArr := NIL;
    t.marked := NIL;
    RETURN t
  END Init;

PROCEDURE Put(t : T; READONLY elem : Elem) : CARDINAL =
  BEGIN
    WITH rep = HnnHrep.New(elem) DO
      RETURN t.putRep(rep)
    END
  END Put;

REVEAL
  Iterator = PubIterator BRANDED Brand & " Iterator" OBJECT
    ri : RepIterator;
  OVERRIDES
    next := IterNext;
  END;

PROCEDURE IterNext(iter : Iterator; VAR e : Elem) : BOOLEAN =
  VAR
    rep : HnnHrep.T;
  BEGIN
    IF iter.ri.next(rep) THEN
      HnnHrep.ToArray(rep, e);
      RETURN TRUE
    ELSE
      RETURN FALSE
    END
  END IterNext;
  
PROCEDURE IterClose(t : T;
                    READONLY elem : Elem; maxHamming : CARDINAL) : Iterator =
  BEGIN
    RETURN NEW(Iterator,
               ri := t.iterCloseRep(HnnHrep.New(elem), maxHamming))
  END IterClose;
  
PROCEDURE IterNnOrdered(t             : T;
                        READONLY elem : Elem;
                        n             : CARDINAL;
                        maxHamming    : CARDINAL) : Iterator =
  BEGIN
    RETURN NEW(Iterator,
               ri := t.iterNnOrderedRep(HnnHrep.New(elem), n, maxHamming))
  END IterNnOrdered;
  
PROCEDURE Iter(t : T;
               ) : Iterator =
  BEGIN
    RETURN NEW(Iterator,
               ri := t.iterRep())
  END Iter;
  
PROCEDURE Get(t : T;
              i : CARDINAL;
              VAR elem : Elem) =
  BEGIN
    WITH rep = t.seq.get(i) DO
      HnnHrep.ToArray(rep, elem)
    END
  END Get;
  
PROCEDURE Size(t : T;
               ) : CARDINAL =
  BEGIN
    RETURN t.seq.size()
  END Size;

PROCEDURE GetLen(t : T) : CARDINAL =
  BEGIN
    RETURN t.len
  END GetLen;

PROCEDURE SetS(t : T;
               s : [ 1 .. HnnSettings.MaxS ]) =
  BEGIN
    t.s := s
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
    t.marked := NIL;

    (* make Rep and store it *)
    elem.id := idx;
    EVAL t.set.put(elem, idx);
    t.seq.addhi(elem);
    
    (* dont need to do more right now, just return the index *)
    RETURN idx

  END PutRep;

PROCEDURE GetRep(t : T;
                 i : CARDINAL; VAR elem : HnnHrep.T) =
  BEGIN
    elem := t.seq.get(i)
  END GetRep;

REVEAL
  RepIterator = PubRepIterator BRANDED Brand & " RepIterator" OBJECT
    t : T;
    arr : REF ARRAY OF CardPair.T; (* k1 : dist ; k2 : id *)
    (* if arr is NIL, simply iterate over all the reps *)
    nxt := 0;
    n : CARDINAL;
  OVERRIDES
    next := NextRep;
  END;
  
PROCEDURE IterCloseRep(t          : T;
                       elem       : HnnHrep.T;
                       maxHamming : CARDINAL) : RepIterator =
  BEGIN
    IF NOT t.valid THEN t.rehash() END;

    WITH arr = GetCloseIds(t, elem, maxHamming) DO
      RETURN
        NEW(RepIterator,
            t    := t,
            arr  := arr,
            n    := NUMBER(arr^))
    END
  END IterCloseRep;
  
PROCEDURE MarkCloseReps(t                   : T;
                        x                   : HnnHrep.T;
                        maxDistPerSubstring : CARDINAL) =
  BEGIN
    IF NOT t.valid THEN
      t.rehash()
    END;
    SeekPotentialRepMatches(t, x, maxDistPerSubstring, t.marked)
  END MarkCloseReps;

PROCEDURE GetCloseIds(t : T;
                      x : HnnHrep.T;
                      maxHamming : CARDINAL) : REF ARRAY OF CardPair.T =
  (* 
     k1: dist
     k2: id
  *)
  VAR
    maxDistPerSubstring := maxHamming DIV t.nTabs();
    (* the Lemma is that at least one substring has to have a distance
       no greater than this from the search key.

       Proof: Dirichlet *)
  BEGIN
    t.marked := NEW(CardSetDef.T).init(); (* clear marked set! *)
    
    MarkCloseReps(t, x, maxDistPerSubstring);

    (* now walk all the marks and check every one for meeting the 
       criteria ... *)

    VAR
      iter := t.marked.iterate();
      id : CARDINAL;
      matchSeq := NEW(CardPairSeq.T).init();
      arr : REF ARRAY OF CardPair.T;
    BEGIN
      WHILE iter.next(id) DO
        WITH candRep = t.seq.get(id) DO
          WITH dist = HnnHrep.Distance(candRep, x) DO
            IF dist <= maxHamming THEN
              matchSeq.addhi(CardPair.T { dist, id })
            END
          END
        END
      END;
      arr := NEW(REF ARRAY OF CardPair.T, matchSeq.size());
      FOR i := 0 TO matchSeq.size() - 1 DO
        arr[i] := matchSeq.get(i)
      END;
      CardPairArraySort.Sort(arr^);
      RETURN arr
    END
  END GetCloseIds;
  
PROCEDURE IterRep(t : T) : RepIterator =
  BEGIN
    RETURN
      NEW(RepIterator,
          t    := t,
          arr  := NIL,
          n    := t.seq.size())
  END IterRep;

PROCEDURE NextRep(iter : RepIterator; VAR e : HnnHrep.T) : BOOLEAN =
  BEGIN
    IF iter.arr = NIL THEN
      IF iter.nxt = iter.n THEN
        RETURN FALSE
      ELSE
        e := iter.t.seq.get(iter.nxt);
        INC(iter.nxt);
        RETURN TRUE
      END
    ELSE
      IF iter.nxt = iter.n THEN
        RETURN FALSE
      ELSE
        e := iter.t.seq.get(iter.arr[iter.nxt].k2);
        INC(iter.nxt);
        RETURN TRUE
      END
    END
  END NextRep;

PROCEDURE IterNnOrderedRep(t             : T;
                           elem          : HnnHrep.T;
                           n             : CARDINAL;
                           maxHamming    : CARDINAL) : RepIterator =
  BEGIN
    IF NOT t.valid THEN t.rehash() END;

    VAR
      lim := MIN(maxHamming, t.len);
      mm := ARRAY [0..1] OF CARDINAL { lim DIV 2, lim };
      m : CARDINAL;
    BEGIN
      FOR mi := FIRST(mm) TO LAST(mm) DO
        m := mm[mi];
        WITH arr = GetCloseIds(t, elem, m) DO
          IF m = lim OR NUMBER(arr^) >= n THEN

            Debug.Out(F("InterNnOrderedRep done m %s NUMBER(arr^) %s n %s",
                        Int(m), Int(NUMBER(arr^)), Int(n)));

            FOR i := FIRST(arr^) TO LAST(arr^) DO
              Debug.Out(F("k1 %s k2 %s", Int(arr[i].k1), Int(arr[i].k2)))
            END;
            
            RETURN
              NEW(RepIterator,
                  t    := t,
                  arr  := arr,
                  n    := MIN(n, NUMBER(arr^)))
          END
        END
      END
    END;
    <*ASSERT FALSE*>
  END IterNnOrderedRep;

BEGIN END Hnn.
