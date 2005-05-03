(* $Id$ *)

GENERIC MODULE Cache(Key, Value, KeyRefTbl);

TYPE
  LRU = OBJECT
    which : Key.T;
    prev : LRU;
    next : LRU;
  END;

  S = REF RECORD
    lru : LRU;
    value : Value.T;
  END;

REVEAL
  T = Public BRANDED Brand OBJECT
    maxCache : CARDINAL;
    data : KeyRefTbl.T;
    lru : LRU;
  OVERRIDES
    init := Init;
    get := Get;
    haveCachedData := HaveCachedData;
  END;

PROCEDURE Init(t : T; cacheSize : CARDINAL) : T =
  BEGIN
    t.maxCache := cacheSize;
    t.data := NEW(KeyRefTbl.Default).init(cacheSize);

    (* make sentinel *)
    t.lru := NEW(LRU);
    t.lru.prev := t.lru;
    t.lru.next := t.lru;
    RETURN t
  END Init;

PROCEDURE Get(t : T; idx : Key.T) : Value.T =
  VAR
    res : REFANY;
  BEGIN
    IF t.data.get(idx,res) THEN 
      WITH rec = NARROW(res,S) DO
        (* update LRU *)

        VAR
          l := rec.lru;
        BEGIN
          (* delete l from its old position *)
          l.prev.next := l.next;
          l.next.prev := l.prev;
          
          (* put it at head *)
          l.next := t.lru.next;
          l.prev := t.lru;
          t.lru.next.prev := l;
          t.lru.next := l;
        END;

        RETURN rec.value
      END
    END;

    (* figure out which one to evict, if any *)

    VAR
      l : LRU;
      x : BOOLEAN;
      s : S;
    BEGIN
      <* ASSERT t.data.size() <= t.maxCache *>
      IF t.data.size() = t.maxCache THEN
        (* evict record at tail *)
        l := t.lru.prev;
        t.lru.prev := l.prev;
        l.prev.next := t.lru;

        VAR r : REFANY; BEGIN x := t.data.delete(l.which,r); s:=r END;

        <* ASSERT x *>
      ELSE
        l := NEW(LRU);
        s := NEW(S);
      END;
      
      s.value := t.compute(idx,s.value);
      EVAL t.data.put(idx, s);

      (* update l accordingly *)
      l.which := idx;
      
      (* insert l at head *)
      l.next := t.lru.next;
      l.prev := t.lru;
      
      l.prev.next := l;
      l.next.prev := l;
      
      (* make new record *)
      s.lru := l;
      
      RETURN s.value
    END
  END Get;

PROCEDURE HaveCachedData(t : T; idx : Key.T) : BOOLEAN =
  VAR dummy : REFANY; BEGIN RETURN t.data.get(idx,dummy) END HaveCachedData;

BEGIN END Cache.



