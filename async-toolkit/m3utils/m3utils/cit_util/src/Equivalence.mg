GENERIC MODULE Equivalence(Elem, ElemElemTbl);

TYPE
  Public = T OBJECT METHODS
    init(sizeHint: CARDINAL := 0): Default;
  END;

  PrivateIter = Iterator BRANDED "DefEquivIter(" & Elem.Brand & ")" OBJECT
    iter: ElemElemTbl.Iterator;
  OVERRIDES
    next := Next;
  END;

REVEAL
  Default = Public BRANDED "DefEquiv(" & Elem.Brand & ")" OBJECT
    t: ElemElemTbl.T;
  OVERRIDES
    init := Init;
    equal := Equal;
    identify := Identify;
    canon := Canon;
    iterate := Iterate;
  END;

PROCEDURE Init(self: Default; sizeHint: CARDINAL := 0): Default =
  BEGIN
    self.t := NEW(ElemElemTbl.Default).init(sizeHint);
    RETURN self;
  END Init;

PROCEDURE Equal(self: Default; e1, e2: Elem.T): BOOLEAN =
  BEGIN
    RETURN Elem.Equal(Canon(self, e1), Canon(self, e2));
  END Equal;

PROCEDURE Identify(self: Default; e1, e2: Elem.T): BOOLEAN =
  VAR
    c1 := Canon(self, e1);
    c2 := Canon(self, e2);
  BEGIN
    IF Elem.Equal(c1, c2) THEN
      RETURN TRUE;
    ELSE
      WITH y = NOT self.t.put(c1, c2) DO <* ASSERT y *> END;
      RETURN FALSE;
    END;
  END Identify;

PROCEDURE Canon(self: Default; e: Elem.T): Elem.T =
  VAR
    cur := e;
    len := 0;
  BEGIN
    WHILE self.t.get(cur, cur) DO
      INC(len);
    END;
    (* path compression: *)
    IF len # 0 THEN
      WITH y = self.t.put(e, cur) DO <* ASSERT y *> END;
    END;
    RETURN cur;
  END Canon;

PROCEDURE Iterate(self: Default): Iterator =
  BEGIN
    RETURN NEW(PrivateIter, iter := self.t.iterate());
  END Iterate;

PROCEDURE Next(self: PrivateIter; VAR alias, canon: Elem.T): BOOLEAN =
  BEGIN
    RETURN self.iter.next(alias, canon);
  END Next;

BEGIN
END Equivalence.
