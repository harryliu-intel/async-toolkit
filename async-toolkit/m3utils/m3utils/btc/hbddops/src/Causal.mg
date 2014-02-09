(* $Id$ *)

GENERIC MODULE Causal(Elem, 
                      ElemSet, ElemSetDef,
                      ElemList, 
                      ElemPair,
                      ElemLongrealTbl,
                      ElemPairLongrealTbl, 
                      ElemElemSetTbl);

REVEAL
  T = Public BRANDED Brand OBJECT
    predTbl      : ElemElemSetTbl.T;
    succTbl      : ElemElemSetTbl.T;
    sourceSet    : ElemSet.T;
    sinkSet      : ElemSet.T;
    delayTbl     : ElemPairLongrealTbl.T;
    timeTbl      : ElemLongrealTbl.T;
    lastElem     : Elem.T;
    lastTime     := FIRST(LONGREAL);
  METHODS
    visit        (e : Elem.T)   := Visit;

    traverse (e : Elem.T)   := Traverse;   
    (* search ancestors from a node *)

    recalculate(e : Elem.T) := Recalculate;
    (* update descendants from a node *)

  OVERRIDES
    init          := Init;
    last          := Last;
    time          := Time;

    addDependency := AddDependency;
    delDependency := DelDependency;
    changeDelay   := ChangeDelay;
  END;

PROCEDURE Init(t : T) : T =
  BEGIN
    t.predTbl     := NEW(ElemElemSetTbl.Default).init();
    t.succTbl     := NEW(ElemElemSetTbl.Default).init();
    t.sourceSet   := NEW(ElemSetDef.T).init();
    t.sinkSet     := NEW(ElemSetDef.T).init();
    t.delayTbl    := NEW(ElemPairLongrealTbl.Default).init();
    t.timeTbl     := NEW(ElemLongrealTbl.Default).init();
    RETURN t
  END Init;

PROCEDURE Time(t : T; e : Elem.T) : LONGREAL =
  VAR
    dummy : ElemSet.T;
    tm : LONGREAL;
  BEGIN
    IF NOT t.predTbl.get(e, dummy) THEN
      t.traverse(e)
    END;
    WITH found = t.timeTbl.get(e, tm) DO
      <*ASSERT found*>
      RETURN tm
    END
  END Time;

PROCEDURE Traverse(t : T; e : Elem.T) = 
  VAR
    at : LONGREAL;
    p  : Elem.T;
    ps : ElemSet.T;
    ss : ElemSet.T;
  BEGIN
    (* if its already done we do nothing *)
    IF NOT t.predTbl.get(e, ps) THEN
      IF t.isSource(e, at) THEN
        EVAL t.sourceSet.insert(e);
        EVAL t.timeTbl.put(e, at);
        ps := NEW(ElemSetDef.T).init(); (* empty set*)
      ELSE
        ps := t.predecessors(e);
        <*ASSERT ps.size() # 0*>
      END;
      EVAL t.predTbl.put(e, ps);
      WITH iter = ps.iterate() DO
        WHILE iter.next(p) DO
          EVAL t.sinkSet.delete(p);
          IF NOT t.succTbl.get(p, ss) THEN
            ss := NEW(ElemSetDef.T).init();
            EVAL t.succTbl.put(p, ss)
          END;
          EVAL ss.insert(e);
          t.traverse(p)
        END
      END;
      IF NOT t.succTbl.get(e, ss) THEN
        ss := NEW(ElemSetDef.T).init();
        EVAL t.succTbl.put(e, ss)
      END
    END;
    t.visit(e) (* in postorder *)
  END Traverse;

PROCEDURE Visit(t : T; e : Elem.T) =
  VAR 
    max := FIRST(LONGREAL);
    ps : ElemSet.T;
    p : Elem.T;
    dly : LONGREAL;
  BEGIN
    WITH found = t.predTbl.get(e, ps),
         iter  = ps.iterate() DO
      <*ASSERT found*>
      WHILE iter.next(p) DO
        IF NOT t.delayTbl.get(ElemPair.T { p, e }, dly) THEN
          dly := t.delay(p, e);
          EVAL t.delayTbl.put(ElemPair.T { p, e }, dly)
        END;
        WITH this = dly + t.time(p) DO
          IF this > max THEN max := this END;
        END;
        EVAL t.timeTbl.put(e, max);
        IF max > t.lastTime THEN
          t.lastTime := max;
          t.lastElem := e
        END
      END
    END
  END Visit;

PROCEDURE Last(t : T; VAR at : Elem.T) : LONGREAL =
  VAR
    q  : Elem.T;
    tm : LONGREAL;
  BEGIN
    IF t.lastTime # FIRST(LONGREAL) THEN
      at := t.lastElem;
      RETURN t.lastTime
    ELSE
      WITH iter = t.sinkSet.iterate() DO
        tm := FIRST(LONGREAL);
        WHILE iter.next(q) DO
          WITH qt = t.time(q) DO
            IF qt > tm THEN tm := qt; at := q END
          END
        END
      END;
      t.lastElem := at;
      t.lastTime := tm;
      RETURN tm
    END
  END Last;

PROCEDURE AddDependency(t : T; a, b : Elem.T) =
  VAR
    s : ElemSet.T;
  BEGIN
    WITH found = t.succTbl.get(a, s) DO
      IF NOT found THEN s := NEW(ElemSetDef.T).init() END;
      EVAL s.insert(b)
    END;
    WITH found = t.predTbl.get(a, s) DO
      IF NOT found THEN s := NEW(ElemSetDef.T).init() END;
      EVAL s.insert(a)
    END;
    t.recalculate(b)
  END AddDependency;

PROCEDURE DelDependency(t : T; a, b : Elem.T) =
  VAR
    s : ElemSet.T;
  BEGIN
    WITH found = t.succTbl.get(a, s) DO
      <*ASSERT found*>
      WITH x = s.insert(b) DO <*ASSERT x*> END
    END;
    WITH found = t.predTbl.get(a, s) DO
      <*ASSERT found*>
      WITH x = s.delete(a) DO <*ASSERT x*> END
    END;
    t.recalculate(b)
  END DelDependency;

PROCEDURE Recalculate(t : T; b : Elem.T) =
  VAR
    t1 := FIRST(LONGREAL);
    t2 : LONGREAL;
    s : ElemSet.T;
    c : Elem.T;
  BEGIN
    EVAL t.timeTbl.get(b,t1);
    t.traverse(b);
    WITH found = t.timeTbl.get(b,t2) DO
      <*ASSERT found*>
      IF t1 # t2 THEN
        WITH x = t.succTbl.get(b,s),
             iter = s.iterate() DO
          <*ASSERT x*>
          WHILE iter.next(c) DO
            t.recalculate(c)
          END
        END
      END
    END
  END Recalculate;

PROCEDURE ChangeDelay(t : T; a, b : Elem.T; dly : LONGREAL) = 
  BEGIN
    WITH hadIt = t.delayTbl.put(ElemPair.T { a, b }, dly) DO
      <*ASSERT hadIt*>
      t.recalculate(b)
    END
  END ChangeDelay;

BEGIN END Causal.
