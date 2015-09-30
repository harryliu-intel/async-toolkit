MODULE MetaUtils;
IMPORT NameSetDef, NameSet, SchemePair;
IMPORT Name, SchemeUtils, NameRefTbl, RefList;
IMPORT NameNameSetTbl;

PROCEDURE NRKeysToSet(tbl : NameRefTbl.T) : NameSet.T =
  VAR set := NEW(NameSetDef.T).init();
      n : Name.T;
      r : REFANY;
  BEGIN
    WITH iter = tbl.iterate() DO
      WHILE iter.next(n,r) DO EVAL set.insert(n) END
    END;
    RETURN set
  END NRKeysToSet;

PROCEDURE MatchingRoots(set : NameSet.T; root : Name.T) : NameSet.T =
  VAR res := NEW(NameSetDef.T).init();
      n : Name.T;
  BEGIN
    WITH iter = set.iterate() DO
      WHILE iter.next(n) DO 
        VAR nn := n; BEGIN
          WHILE nn # NIL DO 
            IF nn = root THEN EVAL res.insert(n); EXIT END;
            nn := Name.Parent(nn)
          END
        END
      END
    END;
    RETURN res
  END MatchingRoots;

PROCEDURE SetToList(set : NameSet.T) : SchemePair.T =
  VAR res : SchemePair.T := NIL;
      n : Name.T;
  BEGIN
    WITH iter = set.iterate() DO
      WHILE iter.next(n) DO 
        res := SchemeUtils.Cons(n,res)
      END
    END;
    RETURN res
  END SetToList;

PROCEDURE RefListToList(lst : RefList.T) : SchemePair.T =
  VAR 
    sentinel :=  NEW(SchemePair.T, first := NIL, rest := NIL);
    p := sentinel;
  BEGIN
    WHILE lst # NIL DO
      p.rest := NEW(SchemePair.T, first := lst.head, rest := NIL);
      p := p.rest;
      lst := lst.tail
    END;
    RETURN sentinel.rest
  END RefListToList;

PROCEDURE MakeSet(p : SchemePair.T) : NameSet.T =
  VAR  res := NEW(NameSetDef.T).init();
  BEGIN
    WHILE p # NIL DO
      EVAL res.insert(p.first);
      p := p.rest
    END;
    RETURN res
  END MakeSet;

PROCEDURE ListListToAliasTbl(p : SchemePair.T) : NameNameSetTbl.T =
  VAR res := NEW(NameNameSetTbl.Default).init();
  BEGIN
    WHILE p # NIL DO
      WITH set = MakeSet(p.first) DO
        VAR q : SchemePair.T := p.first; BEGIN
          WHILE q # NIL DO
            EVAL res.put(q.first,set);
            q := q.rest
          END
        END
      END;
      p := p.rest;
    END;
    RETURN res
  END ListListToAliasTbl;

BEGIN END MetaUtils.
