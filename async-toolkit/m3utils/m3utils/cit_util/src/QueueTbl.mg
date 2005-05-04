GENERIC MODULE QueueTbl(Tbl, Key, Value, KeyRefTbl);
IMPORT IntPQ;


REVEAL
  T = Public BRANDED OBJECT
    curP := 0;
    pq: IntPQ.Default := NIL;
    getElt: KeyRefTbl.T;

  OVERRIDES
    iterateQOrdered := Iterate;
    put             := Put;
    delete          := Delete;
  END;

PROCEDURE Init(self: T) =
  BEGIN
    IF self.pq = NIL THEN
      self.pq := NEW(IntPQ.Default).init();
      self.getElt := NEW(KeyRefTbl.Default).init();
    END;
  END Init;

PROCEDURE Put(self: T; READONLY k: Key.T; READONLY v: Value.T): BOOLEAN =
  BEGIN
    Init(self);
    IF Public.put(self, k, v) THEN
      RETURN TRUE;
    ELSE
      WITH e = NEW(Elt, priority:=self.curP, k:=k) DO
        self.pq.insert(e);
        EVAL self.getElt.put(k, e);
      END;
      INC(self.curP);
      RETURN FALSE;
    END;
  END Put;

PROCEDURE Delete(self: T; READONLY k: Key.T; VAR v: Value.T): BOOLEAN =
  BEGIN
    Init(self);
    IF self.get(k, v) THEN
      VAR
        e: REFANY;
      BEGIN
        IF NOT self.getElt.get(k, e) THEN
          <* ASSERT FALSE *>
        END;
        TRY
          self.pq.delete(e);
        EXCEPT IntPQ.NotInQueue =>
          <* ASSERT FALSE *>
        END;
      END;
      RETURN self.delete(k, v);
    END;
    RETURN FALSE;
  END Delete;

TYPE
  Elt = IntPQ.Elt OBJECT
    k: Key.T;
  END;

  Iterator = Tbl.Iterator OBJECT
    self: T;
    elts: REF ARRAY OF IntPQ.Elt;
    i := 0;
  OVERRIDES
    next := Next;
  END;

PROCEDURE Iterate(self: T): Tbl.Iterator =
  BEGIN
    IF self.pq = NIL THEN RETURN self.iterate(); END;
    WITH elts = NEW(REF ARRAY OF IntPQ.Elt, self.pq.size()) DO

      (* get the sorted elts *)
      TRY
        FOR i := 0 TO LAST(elts^) DO
          elts[i] := self.pq.deleteMin();
        END;
      EXCEPT IntPQ.Empty =>
        <* ASSERT FALSE *>
      END;

      (* restore self.pq *)
      self.pq := self.pq.fromArray(elts^);

      (* return an iterator *)
      RETURN NEW(Iterator, self := self, elts := elts);
    END;
  END Iterate;

PROCEDURE Next(self: Iterator; VAR k: Key.T; VAR v: Value.T): BOOLEAN =
  BEGIN
    IF self.i >= NUMBER(self.elts^) THEN
      RETURN FALSE;
    END;
    k := NARROW(self.elts[self.i], Elt).k;
    IF NOT self.self.get(k, v) THEN
      <* ASSERT FALSE *>
    END;
    INC(self.i);
    RETURN TRUE;
  END Next;


BEGIN
END QueueTbl.
