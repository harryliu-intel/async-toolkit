(* $Id$ *)

GENERIC MODULE Causal(Elem, 
                      ElemSet, ElemSetDef,
                      ElemLongrealTbl,
                      ElemElemSetTbl,
                      ElemSeq,
                      ElemRefTbl);

IMPORT Debug, Wx;
FROM Fmt IMPORT LongReal, F, Int, Bool;

CONST Verbose = FALSE;

REVEAL
  T = Public BRANDED Brand OBJECT
    predTbl      : ElemRefTbl.T;
    succTbl      : ElemElemSetTbl.T;
    sourceSet    : ElemSet.T;
    timeTbl      : ElemLongrealTbl.T;
    lastElem     : Elem.T;
    lastTime     := FIRST(LONGREAL);
    toSync       : ElemSet.T;

  OVERRIDES
    init          := Init;
    last          := Last;
    time          := Time;

    addDependency := AddDependency;
    delDependency := DelDependency;
    setDelay      := SetDelay;
    sync          := Sync;
    debugFmt      := DefDebugFmt;
    
    successors    := SuccSet;

    findCriticalInput := FindCriticalInput;
    
    setSource     := SetSource;
    predecessors  := Predecessors;
    getDelay      := GetDelay;
  END;

PROCEDURE SetSource(t : T; e : Elem.T; at : LONGREAL) =
  BEGIN
    EVAL t.sourceSet.insert(e);
    EVAL t.timeTbl.put(e, at)
  END SetSource;

TYPE
  Predecessor = RECORD
    p : Elem.T;           (* identity of predecessor *)
    d := Uninitialized;   (* delay from predecessor *)
  END;

  Preds = ARRAY OF Predecessor;

CONST Uninitialized = FIRST(LONGREAL);

PROCEDURE FindCriticalInput(t : T; e : Elem.T; VAR crit : Elem.T) : BOOLEAN =
  VAR
    tt := FIRST(LONGREAL);
  BEGIN
    CreateIfNotExist(t, e);

    WITH preds = PredSet(t, e)^ DO
      FOR i := FIRST(preds) TO LAST(preds) DO
        WITH p = preds[i].p DO
          IF preds[i].d = Uninitialized THEN
            <*ASSERT FALSE*>
            (*preds[i].d := t.delay(p, e)*)
          END;
          WITH pt = t.time(p),
               it = pt + preds[i].d DO
            IF it > tt THEN
              tt := it;
              crit := p
            END
          END
        END
      END
    END;
    RETURN tt # FIRST(LONGREAL)
  END FindCriticalInput;

PROCEDURE SelectOne(s       : ElemSet.T; 
                    VAR res : Elem.T;
                    notIn   : ElemSet.T := NIL) : BOOLEAN =
  VAR 
    e : Elem.T;
    iter := s.iterate();
  BEGIN
    WHILE iter.next(e) DO
      IF notIn = NIL OR NOT notIn.member(e) THEN res := e; RETURN TRUE END
    END;
    RETURN FALSE
  END SelectOne;

PROCEDURE TopoSort(t : T) : ElemSeq.T =

  (* from Wikipedia *)
  PROCEDURE Visit(n : Elem.T) =
    VAR 
      m : Elem.T;
    BEGIN

      IF tempMarks.member(n) THEN Debug.Error("not a DAG!") END;
      IF toVisit.member(n) THEN
        EVAL tempMarks.insert(n);
        WITH iter = t.successors(n).iterate() DO
          WHILE iter.next(m) DO
            IF Verbose THEN 
              Dbg("TopoSort.Visit", t.debugFmt(n) & "->" & t.debugFmt(m)) 
            END;
            Visit(m)
          END
        END;
        EVAL toVisit.delete(n); EVAL tempMarks.delete(n);
        res.addlo(n)
      END
    END Visit;

  VAR
    res       := NEW(ElemSeq.T).init();
    tempMarks := NEW(ElemSetDef.T).init();
    toVisit   := AllNodes(t);
    n : Elem.T;
  BEGIN
    WHILE SelectOne(toVisit, n, notIn := tempMarks) DO
      Visit(n)
    END;
    IF Verbose THEN
      VAR wx := Wx.New(); BEGIN
        FOR i := 0 TO res.size()-1 DO
          Wx.PutChar(wx, ' ');
          Wx.PutText(wx, t.debugFmt(res.get(i)))
        END;
        Dbg("TopoSort", "returning" & Wx.ToText(wx))
      END
    END;
    RETURN res
  END TopoSort;

PROCEDURE AllNodes(t : T) : ElemSet.T =
  (* find all nodes visible from the sync set *)

  PROCEDURE Check(z : Elem.T) =
    BEGIN
      IF NOT res.member(z) AND NOT ts.member(z) THEN 
        todo.addhi(z) ; EVAL ts.insert(z)
      END
    END Check;

  VAR 
    res, ts := NEW(ElemSetDef.T).init();
    todo := NEW(ElemSeq.T).init();
    iter := t.toSync.iterate();
    e, x : Elem.T;
  BEGIN
    WHILE iter.next(e) DO todo.addhi(e) END;
    
    WHILE todo.size() > 0 DO
      WITH e = todo.remlo() DO
        EVAL res.insert(e);
        
        WITH pa = PredSet(t,e)^ DO
          FOR i := FIRST(pa) TO LAST(pa) DO
            Check(pa[i].p)
          END
        END;

        iter := t.successors(e).iterate();
        WHILE iter.next(x) DO Check(x) END
      END
    END;
    RETURN res
  END AllNodes;

<*NOWARN*>PROCEDURE Error(tag, err : TEXT) =
  BEGIN
    Debug.Error(Brand & "." & tag & " : " & err);
    <*ASSERT FALSE*>
  END Error;

<*NOWARN*>PROCEDURE Warn(tag, err : TEXT) =
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
    t.predTbl     := NEW(ElemRefTbl.Default).init();
    t.succTbl     := NEW(ElemElemSetTbl.Default).init();
    t.sourceSet   := NEW(ElemSetDef.T).init();
    t.timeTbl     := NEW(ElemLongrealTbl.Default).init();
    t.toSync      := NEW(ElemSetDef.T).init();
    RETURN t
  END Init;

PROCEDURE Time(t : T; e : Elem.T) : LONGREAL =
  VAR
    tm : LONGREAL;
  BEGIN
    CreateIfNotExist(t, e);
    WITH found = t.timeTbl.get(e,tm) DO <*ASSERT found*> END;
    RETURN tm
  END Time;

PROCEDURE SuccSet(t : T; p : Elem.T) : ElemSet.T =
  VAR ss : ElemSet.T;
  BEGIN
    IF NOT t.succTbl.get(p, ss) THEN
      ss := NEW(ElemSetDef.T).init();
      EVAL t.succTbl.put(p, ss)
    END;
    RETURN ss
  END SuccSet;

PROCEDURE PredSet(t : T; p : Elem.T) : REF Preds =
  VAR 
    ss : REFANY;
  BEGIN
    IF Verbose THEN
      Dbg("PredSet",t.debugFmt(p))
    END;
    WITH hadIt = t.predTbl.get(p, ss) DO
      <*ASSERT hadIt*>
    END;
    RETURN ss
  END PredSet;

PROCEDURE Predecessors(t : T; e : Elem.T) : ElemSet.T =
  VAR res := NEW(ElemSetDef.T).init();
  BEGIN
    WITH p = PredSet(t, e)^ DO
      FOR i := FIRST(p) TO LAST(p) DO
        EVAL res.insert(p[i].p)
      END
    END;
    RETURN res
  END Predecessors;

PROCEDURE Last(t : T; VAR at : Elem.T) : LONGREAL =
  BEGIN
    IF t.lastTime # FIRST(LONGREAL) THEN
      at := t.lastElem;
      RETURN t.lastTime
    ELSE
      <*ASSERT FALSE*>
    END
  END Last;

PROCEDURE AddDependency(t : T; a, b : Elem.T; sync : BOOLEAN) =
  BEGIN
    <*ASSERT NOT t.sourceSet.member(b)*>
    IF Verbose THEN
      Dbg("AddDependency", t.debugFmt(a) & " -> " & t.debugFmt(b))
    END;
    EVAL SuccSet(t, a).insert(b);

    InsertInPreds(t, a, b);
      
    EVAL t.toSync.insert(b);
    IF sync THEN t.sync() END;

  END AddDependency;

PROCEDURE InsertInPreds(t : T; a (* pred *), b (* succ *) : Elem.T) =
  (* add a to pred list of b *)
  VAR
    pst : REF Preds;
    np : CARDINAL;
  BEGIN
    CreateIfNotExist(t, b);
    pst := PredSet(t, b);
    np := NUMBER(pst^);
    
    (* check if we already have it -- necessary ? *)
    FOR i := FIRST(pst^) TO LAST(pst^) DO
      IF Elem.Equal(pst[i].p ,a) THEN
        RETURN
      END
    END;
    
    WITH new = NEW(REF Preds, np+1) DO
      SUBARRAY(new^,0,np) := pst^;
      new[np].p := a;
      EVAL t.predTbl.put(b, new)
    END;

    CreateIfNotExist(t, a)
  END InsertInPreds;

PROCEDURE DeleteFromPreds(t : T; a (* pred *), b (* succ *) : Elem.T) =
  (* delete a from pred list of b *)
  VAR
    pst : REF Preds;
    np : CARDINAL;
  BEGIN
    pst := PredSet(t, b);
    np := NUMBER(pst^);
    FOR i := FIRST(pst^) TO LAST(pst^) DO
      IF Elem.Equal(pst[i].p ,a) THEN
        (* swap to end and dealloc *)
        WITH new = NEW(REF Preds, np-1) DO
          SUBARRAY(new^, 0, i) := SUBARRAY(pst^, 0, i);
          SUBARRAY(new^, i, np-i-1) := SUBARRAY(pst^, i+1, np-i-1);
          EVAL t.predTbl.put(b, new)
        END;
        EXIT
      END
    END
  END DeleteFromPreds;

PROCEDURE DelDependency(t : T; a, b : Elem.T; sync : BOOLEAN) =
  BEGIN
    IF Verbose THEN
      Dbg("DelDependency", t.debugFmt(a) & " -> " & t.debugFmt(b))
    END;
    EVAL SuccSet(t, a).delete(b);

    DeleteFromPreds(t, a, b);

    EVAL t.toSync.insert(b);
    IF sync THEN t.sync() END
  END DelDependency;

PROCEDURE CreateIfNotExist(t : T; e : Elem.T) =
  (* create pred and succ data structures for e if they dont already
     exist, recursively creating those for all predecessors

     also fill in delayTbl if created. 
  *)
  VAR
    ps : REFANY;
  BEGIN
    IF NOT t.predTbl.get(e, ps) THEN
      EVAL t.predTbl.put(e, NEW(REF Preds, 0))
    END;
    EVAL SuccSet(t, e)
  END CreateIfNotExist;

PROCEDURE CalcTime(t : T; e : Elem.T; VAR pst : Preds) : BOOLEAN =
  VAR max := FIRST(LONGREAL);
      old := FIRST(LONGREAL);
  BEGIN
    EVAL t.timeTbl.get(e, old);

    FOR i := FIRST(pst) TO LAST(pst) DO

      IF pst[i].d = Uninitialized THEN
        <*ASSERT FALSE*>
        (*pst[i].d := t.delay(pst[i].p, e)*)
      END;

      WITH int = t.time(pst[i].p),
           d   = pst[i].d,
           this = d + int DO
        IF Verbose THEN
          Dbg("CalcTime", F("%s @%s + %s -> %s @%s",
                            t.debugFmt(pst[i].p),
                            LongReal(int),
                            LongReal(d),
                            t.debugFmt(e),
                            LongReal(this)))
        END;
        IF this > max THEN max := this END;
      END;

      IF max > t.lastTime THEN
        t.lastTime := max;
        t.lastElem := e
      ELSIF e = t.lastElem THEN
        (* mark last data stale *)
        t.lastTime := FIRST(LONGREAL)
      END;
    END;

    IF Verbose THEN
      Dbg("CalcTime", t.debugFmt(e) & 
        " max=" & LongReal(max) & 
        " old=" & LongReal(old) & 
        " old=max =" & Bool(old=max) )
    END;
    
    WITH res = old # max DO
      IF res THEN
        EVAL t.timeTbl.put(e, max);

        IF Verbose THEN
            Dbg("CalcTime", F("updating time(%s) %s -> %s",
                              t.debugFmt(e),
                              LongReal(old), LongReal(max)))
        END
      END;
      RETURN res
    END
  END CalcTime;

PROCEDURE Sync(t : T) =
  (* recalculate everything that is stale *)

  PROCEDURE Recalc(e : Elem.T) =
    VAR s : Elem.T;
    BEGIN
      IF Verbose THEN
        Dbg("Sync, recalculating ", t.debugFmt(e))
      END;

      WITH preds = PredSet(t, e)^ DO 
        IF CalcTime(t, e, preds) THEN
          WITH succi = SuccSet(t, e).iterate() DO
            WHILE succi.next(s) DO
              EVAL t.toSync.insert(s)
            END
          END
        END
      END
    END Recalc;

  BEGIN
    IF Verbose THEN
      Dbg("Sync", "********  " & Int(t.toSync.size()) & " events")
    END;

    (* sync the nodes in topological sort order *)
    WITH topo = TopoSort(t) DO
      FOR i := 0 TO topo.size()-1 DO
        WITH e = topo.get(i) DO
          IF t.toSync.member(e) THEN Recalc(e); EVAL t.toSync.delete(e) END
        END
      END
    END
  END Sync;

PROCEDURE SetDelay(t : T; a, b : Elem.T; dly : LONGREAL; sync : BOOLEAN) = 
  VAR
    ps : REFANY;
    found := FALSE;
  BEGIN
    WITH hadIt = t.predTbl.get(b, ps) DO
      <*ASSERT hadIt*>
      WITH pst = NARROW(ps, REF Preds)^ DO
        FOR i := FIRST(pst) TO LAST(pst) DO
          IF pst[i].p = a THEN
            found := TRUE;
            pst[i].d := dly;
            EXIT
          END
        END
      END
    END;
    <*ASSERT found*>

    EVAL t.toSync.insert(b);
    IF sync THEN t.sync() END

  END SetDelay;

PROCEDURE GetDelay(t : T; a, b : Elem.T) : LONGREAL =
  BEGIN
    IF Verbose THEN
      Dbg("GetDelay", t.debugFmt(a) & " -> " & t.debugFmt(b))
    END;

    WITH p = PredSet(t, b)^ DO
      FOR i := FIRST(p) TO LAST(p) DO
        IF Elem.Equal(p[i].p,a) THEN
          RETURN p[i].d
        END
      END
    END;
    <*ASSERT FALSE*>
  END GetDelay;

BEGIN END Causal.
