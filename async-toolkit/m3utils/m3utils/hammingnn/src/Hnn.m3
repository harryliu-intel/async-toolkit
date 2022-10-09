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
IMPORT CardList;
IMPORT Word;
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

    hashTabs : HashTabs;
    (* hashTabs is in size:
       CEILING(len/s)    x    2^s    x (sequences)
       ^                      ^
       pos in word            bin val of substr

       cardseq is index into t.seq // see above
    *)

    markArray : REF ARRAY OF Word.T;
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
  CardBase = CardList.T;
  HashTabs = REF ARRAY OF ARRAY OF CardBase;

  (* the HashTabs are probably mostly empty?  Or maybe they aren't... 
     If they are loaded, then perhaps CardSeq.T is a more reasonable
     representation than CardList, since it extends more efficiently.
     We can choose dynamically, I suppose...?
  *)

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
    res := NEW(HashTabs, tabls, bucks);
    FOR i := FIRST(res^) TO LAST(res^) DO
      FOR j := FIRST(res[0]) TO LAST(res[0]) DO
        res[i, j] := NIL
      END
    END;
    RETURN res
  END NewHashTabs;
  
PROCEDURE Rehash(t : T) =
  VAR
    r : HnnHrep.T;
    b : Word.T;
  BEGIN
    (* we should really be able to pick s automatically... *)
    <*ASSERT t.s # 0*>

    WITH ntabls = t.nTabs(),
         nbucks = t.nBucks() DO
      t.hashTabs := NewHashTabs(ntabls, nbucks);
      
      FOR i := 0 TO t.seq.size() - 1 DO
        r := t.seq.get(i);
        FOR w := 0 TO ntabls - 1 DO
          b := HnnHrep.GetBits(r, w * t.s, t.s);
          t.hashTabs[w, b] := CardList.Cons(i, t.hashTabs[w, b])
        END
      END
      
    END;

    (* also initialize markArray *)
    t.markArray := NEW(REF ARRAY OF Word.T,
                       (t.seq.size() - 1) DIV Word.Size + 1)
  END Rehash;

PROCEDURE SeekMatches(w            : Word.T;
                      (* this is the word of which we seek matches *)

                      n            : CARDINAL;
                      (* this is the width of the data in the word *)
                      
                      maxDist      : CARDINAL;
                      (* this is the maximum Hamming distance of a match *)

                      READONLY tbl : ARRAY OF CardBase;
                      (* these are the hash buckets *)
                      
                      VAR mark     : ARRAY OF Word.T;
                      (* if we find a match, we mark this array *)
                      
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
      WHILE p # NIL DO
        WITH id  = p.head,
             wid = id DIV Word.Size DO
          mark[wid] := Word.Or(mark[wid], Word.Shift(1, id MOD Word.Size))
        END;
        p := p.tail
      END
    END MarkBuckets;

  PROCEDURE Recurse(w        : Word.T;
                    maxDist  : CARDINAL;
                    startBit : CARDINAL) =
    BEGIN
      MarkBuckets(w);
      FOR b := startBit TO MIN(startBit + maxDist, n) DO
        WITH ww = Word.Xor(w, Word.Shift(1, b)) DO
          Recurse(ww, maxDist - 1, startBit + 1)
        END
      END
    END Recurse;
    
  BEGIN
    Recurse(w, maxDist, 0)
  END SeekMatches;

PROCEDURE SeekPotentialRepMatches(t                 : T;
                                  rep               : HnnHrep.T;
                                  maxBucketDistance : CARDINAL;
                                  VAR mark          : ARRAY OF Word.T) =
  (* find a global set of potential matches, by marking the mark array, 
     that match at least one bucket in at least maxBucketDistance bits *)
  BEGIN
    WITH ntabls = t.nTabs(),
         nbucks = t.nBucks() DO
      FOR i := 0 TO ntabls - 1 DO
        WITH lob = i * t.s,                   (* first bit pos *)
             nxb = MIN(t.len, (i + 1) * t.s), (* first bit pos not incl *)
             wid = nxb - lob,
             ww = HnnHrep.GetBits(rep, lob, nxb - wid)
         DO
          SeekMatches(ww, wid, maxBucketDistance, t.hashTabs[i], mark)
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
    t.markArray := NIL;
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
              i : CARDINAL; VAR elem : Elem) =
  BEGIN
  END Get;
  
PROCEDURE Size(t : T;
               ) : CARDINAL =
  BEGIN
  END Size;

PROCEDURE GetLen(t : T) : CARDINAL =
  BEGIN
    RETURN t.len
  END GetLen;

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
    t.markArray := NIL;

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
    seq : CardSeq.T;
    (* if seq is NIL, simply iterate over all the reps *)
    nxt := 0;
    n : CARDINAL;
  OVERRIDES
    next := NextRep;
  END;
  
PROCEDURE IterCloseRep(t          : T;
                       elem       : HnnHrep.T;
                       maxHamming : CARDINAL) : RepIterator =
  VAR
    maxDistPerSubstring := maxHamming DIV t.nTabs();
    (* the Lemma is that at least one substring has to have a distance
       no greater than this from the search key.

       Proof: Dirichlet *)
  BEGIN
    MarkCloseReps(t, elem, maxDistPerSubstring);

    (* now walk all the marks and check every one for meeting the 
       criteria ... *)

    (* XXX ok the marks need to be a Set, not a bit vector. *)

    
  END IterCloseRep;

PROCEDURE MarkCloseReps(t                   : T;
                        x                   : HnnHrep.T;
                        maxDistPerSubstring : CARDINAL) =
  (* leaves the candidates in t.mask *)
  BEGIN
    IF NOT t.valid THEN
      t.rehash()
    END;
    FOR i := FIRST(t.markArray^) TO LAST(t.markArray^) DO
      t.markArray[i] := 0
    END;
    SeekPotentialRepMatches(t, x, maxDistPerSubstring, t.markArray^)
  END MarkCloseReps;
  
PROCEDURE IterRep(t : T) : RepIterator =
  BEGIN
    RETURN
      NEW(RepIterator,
          t := t,
          seq := NIL,
          n := t.seq.size())
  END IterRep;

PROCEDURE NextRep(iter : RepIterator; VAR e : HnnHrep.T) : BOOLEAN =
  BEGIN
    IF iter.seq = NIL THEN
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
        e := iter.t.seq.get(iter.seq.get(iter.nxt));
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
  END IterNnOrderedRep;
    





BEGIN END Hnn.
