(* $Id$ *)

GENERIC MODULE Causal(Elem, 
                      ElemSet, ElemSetDef,
                      ElemPair,
                      ElemLongrealTbl,
                      ElemPairLongrealTbl, 
                      ElemElemSetTbl,
                      ElemSeq,
                      ElemRefTbl);

IMPORT Debug;
FROM Fmt IMPORT LongReal, F, Int;

CONST Verbose = FALSE;

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
    toSync       : ElemSet.T;
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
    sync          := Sync;
    debugFmt      := DefDebugFmt;
    
    successors    := SuccSet;

    findCriticalInput := FindCriticalInput;
  END;

PROCEDURE FindCriticalInput(t : T; e : Elem.T; VAR crit : Elem.T) : BOOLEAN =
  VAR
    p  : Elem.T;
    tt := FIRST(LONGREAL);
  BEGIN
    WITH predi = PredSet(t, e).iterate() DO
      WHILE predi.next(p) DO
        WITH pt = t.time(p),
             it = pt + t.delay(p, e) DO
          IF it > tt THEN
            tt := it;
            crit := p
          END
        END
      END
    END;
    RETURN tt # FIRST(LONGREAL)
  END FindCriticalInput;

PROCEDURE Error(tag, err : TEXT) =
  BEGIN
    Debug.Error(Brand & "." & tag & " : " & err);
    <*ASSERT FALSE*>
  END Error;

PROCEDURE Warn(tag, err : TEXT) =
  BEGIN
    Debug.Warning(Brand & "." & tag & " : " & err)
  END Warn;

PROCEDURE Dbg(tag, err : TEXT) =
  BEGIN
    Debug.Out(Brand & "." & tag & " : " & err)
  END Dbg;

PROCEDURE DefDebugFmt(<*UNUSED*>t : T; <*UNUSED*>e : Elem.T) : TEXT =
  BEGIN RETURN "(" & Elem.Brand & ")" END DefDebugFmt;

PROCEDURE Init(t : T) : T =
  BEGIN
    t.predTbl     := NEW(ElemElemSetTbl.Default).init();
    t.succTbl     := NEW(ElemElemSetTbl.Default).init();
    t.sourceSet   := NEW(ElemSetDef.T).init();
    t.sinkSet     := NEW(ElemSetDef.T).init();
    t.delayTbl    := NEW(ElemPairLongrealTbl.Default).init();
    t.timeTbl     := NEW(ElemLongrealTbl.Default).init();
    t.toSync      := NEW(ElemSetDef.T).init();
    RETURN t
  END Init;

PROCEDURE Time(t : T; e : Elem.T) : LONGREAL =
  VAR
    dummy : ElemSet.T;
    tm : LONGREAL;
  BEGIN
    IF NOT t.predTbl.get(e, dummy) OR NOT t.timeTbl.get(e, tm) THEN
      t.traverse(e);
      WITH found = t.timeTbl.get(e,tm) DO
        <*ASSERT found*>
      END
    END;

    RETURN tm
  END Time;

PROCEDURE Traverse(t : T; e : Elem.T) = 
  VAR
    at : LONGREAL;
    p  : Elem.T;
    ps : ElemSet.T;
  BEGIN
    (* if its already done we do nothing -- special handling for creation *)
    IF NOT t.predTbl.get(e, ps) THEN
      IF t.isSource(e, at) THEN
        EVAL t.sourceSet.insert(e);
        IF at > LAST(LONGREAL) / 2.0d0 THEN
          Warn("Traverse", "source very late : " & t.debugFmt(e) & " @ " & 
            LongReal(at))
        END;
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
          EVAL SuccSet(t, p).insert(e);
          t.traverse(p)
        END
      END;
      EVAL SuccSet(t, e);
    END;
    t.visit(e) (* in postorder *)
  END Traverse;

PROCEDURE SuccSet(t : T; p : Elem.T) : ElemSet.T =
  VAR ss : ElemSet.T;
  BEGIN
    IF NOT t.succTbl.get(p, ss) THEN
      ss := NEW(ElemSetDef.T).init();
      EVAL t.succTbl.put(p, ss)
    END;
    RETURN ss
  END SuccSet;

PROCEDURE PredSet(t : T; p : Elem.T) : ElemSet.T =
  VAR ss : ElemSet.T;
  BEGIN
    IF NOT t.predTbl.get(p, ss) THEN
      ss := NEW(ElemSetDef.T).init();
      EVAL t.predTbl.put(p, ss)
    END;
    RETURN ss
  END PredSet;

PROCEDURE Visit(t : T; e : Elem.T) =
  VAR 
    max := FIRST(LONGREAL);
    ps : ElemSet.T;
    p : Elem.T;
    dly : LONGREAL;
  BEGIN

    (* delayTbl is iterated over only like this:
       for a node, iterate over predecessors.

       therefore, delay to be recorded with predecessors *)

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
        IF max > LAST(LONGREAL) / 2.0d0 THEN
          Warn("Visit", "event very late : " & t.debugFmt(e) & " @ " & 
            LongReal(max))
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

PROCEDURE AddDependency(t : T; a, b : Elem.T; sync : BOOLEAN) =
  BEGIN
    IF Verbose THEN
      Dbg("AddDependency", t.debugFmt(a) & " -> " & t.debugFmt(b))
    END;
    EVAL SuccSet(t, a).insert(b);
    EVAL PredSet(t, b).insert(a);
    IF sync THEN
      t.recalculate(b)
    ELSE
      EVAL t.toSync.insert(b)
    END
  END AddDependency;

PROCEDURE DelDependency(t : T; a, b : Elem.T; sync : BOOLEAN) =
  BEGIN
    IF Verbose THEN
      Dbg("DelDependency", t.debugFmt(a) & " -> " & t.debugFmt(b))
    END;
    (* clearly the whole graph need not be built ... *)
    EVAL SuccSet(t, a).delete(b);
    EVAL PredSet(t, b).delete(a);
    IF sync THEN
      t.recalculate(b)
    ELSE
      EVAL t.toSync.insert(b)
    END
  END DelDependency;

PROCEDURE Sync(t : T) =
  (* recalculate everything that is stale *)
  BEGIN
    IF Verbose THEN
      Dbg("Sync", "********  " & Int(t.toSync.size()) & " events")
    END;

    WHILE t.toSync.size() # 0 DO
      VAR iter := t.toSync.iterate();
          e : Elem.T;
      BEGIN
        EVAL iter.next(e);
        t.recalculate(e)
      END
    END
  END Sync;

PROCEDURE Recalculate(t : T; b : Elem.T) =
  VAR
    t1 := FIRST(LONGREAL);
    t2 : LONGREAL;
    c : Elem.T;
  BEGIN
    EVAL t.toSync.delete(b);
    EVAL t.timeTbl.get(b,t1);
    t.traverse(b);
    WITH found = t.timeTbl.get(b,t2) DO
      <*ASSERT found*>
      IF t1 # t2 THEN
        WITH s    = SuccSet(t, b),
             iter = s.iterate() DO
          IF Verbose THEN
            Dbg("Recalculate", F("updating time(%s) %s -> %s, %s successor(s)",
                                 t.debugFmt(b),
                                 LongReal(t1), LongReal(t2),
                                 Int(s.size())))
          END;
          WHILE iter.next(c) DO t.recalculate(c) END
        END
      END
    END
  END Recalculate;

PROCEDURE ChangeDelay(t : T; a, b : Elem.T; dly : LONGREAL; sync : BOOLEAN) = 
  BEGIN
    WITH hadIt = t.delayTbl.put(ElemPair.T { a, b }, dly) DO
      <*ASSERT hadIt*>
      IF sync THEN
        t.recalculate(b)
      ELSE
        EVAL t.toSync.insert(b)
      END
    END
  END ChangeDelay;

BEGIN END Causal.
