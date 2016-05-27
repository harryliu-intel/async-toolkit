MODULE Schmoozer;
IMPORT Schmooze;
IMPORT RefSeq;
IMPORT Dims;
IMPORT Debug;
IMPORT RefList;
IMPORT FS;
IMPORT Time, Date;
FROM Fmt IMPORT Int, LongReal, F;
IMPORT LRSeqRefTbl AS PointRefTbl;
IMPORT ProcUtils;
IMPORT Pathname;
IMPORT CardRefTbl;
IMPORT Text;
IMPORT Rd, FileRd, Scan;
IMPORT Thread;
IMPORT Wr, FileWr;
IMPORT TextUtils;
IMPORT OSError, AL;
IMPORT XYList;
IMPORT Lex, FloatMode;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT NbCommand, NbCommandSeq;
IMPORT Env;
IMPORT IntSetDef, IntSet;
IMPORT TextRd;
IMPORT TextReader;
IMPORT CardSetDef;
IMPORT TextCardTbl;
IMPORT LongRealSeq AS LRSeq;
IMPORT LongRealSeq AS Point;
IMPORT LRSeqSeq AS PointSeq;
IMPORT MeshNode, MeshNodeList;
IMPORT Word;

<*FATAL Thread.Alerted*>
<*FATAL Wr.Failure*>

CONST TE = Text.Equal;
CONST NbTimeout = 60.0d0; (* timeout for all NB commands *)

REVEAL
  Param = PublicParam BRANDED OBJECT
  OVERRIDES
    init := InitParam;
  END;

VAR
  allParams := NEW(RefSeq.T).init();

PROCEDURE InitParam(p : Param) : Param = 
  BEGIN
    allParams.addhi(p);
    RETURN p
  END InitParam;
  
PROCEDURE RT(READONLY z : TA) : REF TA =
  VAR
    res := NEW(REF TA, NUMBER(z));
  BEGIN
    res^ := z;
    RETURN res
  END RT;

PROCEDURE RL(READONLY z : LA) : REF LA =
  VAR
    res := NEW(REF LA, NUMBER(z));
  BEGIN
    res^ := z;
    RETURN res
  END RL;

PROCEDURE RP(READONLY z : PA) : REF PA =
  VAR
    res := NEW(REF PA, NUMBER(z));
  BEGIN
    res^ := z;
    RETURN res
  END RP;

REVEAL
  Variety =  SingleSettings OBJECT
    cover : REF TA; END BRANDED OBJECT
  OVERRIDES 
    n := NVariety;
  END;

  Sweep =  SingleSettings OBJECT
    min, max, step : LONGREAL; END BRANDED OBJECT
  OVERRIDES 
    n := NSweep;
  END;

  SweepSpecific =  SingleSettings OBJECT
    v : REF ARRAY OF LONGREAL; END BRANDED OBJECT
  OVERRIDES 
    n := NSweepSpecific;
  END;

  Schmoo =  PublicSchmoo BRANDED OBJECT
  OVERRIDES
    n := NSchmoo;
  END;

TYPE
  Val = OBJECT
    s : Settings;
  METHODS
    format(tool : Tool) : TEXT := FormatV;
  END;

  StepVal = Val OBJECT i : CARDINAL END;

  SchmooVal = Val OBJECT x : Point.T END;

  SchmooInstance = OBJECT
    schmoo     : Schmoo;
    meshTab    : PointRefTbl.T;
    valPfx     : REF ARRAY OF Val;
    minSquares : XYList.T := NIL;
  END;

VAR schmooInstances : RefList.T := NIL;

PROCEDURE FormatV(v : Val; tool : Tool) : TEXT =
  VAR 
    str : TEXT;
  BEGIN
    TYPECASE v OF  (* ugly *)
      StepVal(st) =>
      TYPECASE st.s OF 
        Variety(v) => 
        <*ASSERT v # NIL*>
        <*ASSERT st # NIL*>
        str := v.cover[st.i]
      |
        Sweep(w) => 
        WITH lr = MIN(w.min + FLOAT(st.i,LR)*w.step, w.max) DO
          str := LongReal(lr)
        END
      |
        SweepSpecific(w) => 
        WITH lr = w.v[st.i] DO
          str := LongReal(lr)
        END
      ELSE
        <*ASSERT FALSE*>
      END;
      WITH ss = NARROW(st.s,SingleSettings) DO
        IF tool = ss.param.tool THEN
          RETURN F("-%s %s", ss.param.flag, str)
        ELSE
          RETURN ""
        END
      END
    |
      SchmooVal(sc) =>
      VAR
        ss := NARROW(sc.s, Schmoo);
        str := "";
        first := TRUE;
      BEGIN
        FOR q := FIRST(ss.param^) TO LAST(ss.param^) DO
          IF ss.param[q].tool = tool THEN
            IF NOT first THEN str := str & " " END;
            str := str & F("-%s %s", ss.param[q].flag, LongReal(FromList(sc.x)[q]));
            first := FALSE
          END
        END;
        RETURN str
      END 
    ELSE
      <*ASSERT FALSE*>
    END
  END FormatV;

PROCEDURE NVariety(v : Variety) : CARDINAL = 
  BEGIN RETURN NUMBER(v.cover^) END NVariety;

PROCEDURE NSweep(s : Sweep) : CARDINAL =
  BEGIN RETURN CEILING((s.max-s.min)/s.step)+1 END NSweep;

PROCEDURE NSweepSpecific(s : SweepSpecific) : CARDINAL =
  BEGIN RETURN NUMBER(s.v^) END NSweepSpecific;

PROCEDURE NSchmoo(<*UNUSED*>s : Schmoo) : CARDINAL = BEGIN RETURN 1 END NSchmoo;

REVEAL
  MeshNode.T = BRANDED OBJECT
    id    : CARDINAL;
    x     : Point.T;
    state := NodeState.Running; (* ?? *)
    watchers : RefList.T := NIL;
    args  : REF ARRAY OF Val;
  METHODS
    init() : MeshNode.T := InitMN;
  END;

TYPE
  NodeState = {  Running ,  Fail ,  Pass ,  ErrorExit  };

CONST NodeStateNames = ARRAY NodeState OF TEXT {
                "Running", "FAIL", "PASS", "ErrorExit" };

TYPE
  PWatcher = OBJECT
    id     : CARDINAL;
    s      : SchmooInstance;
    done   := FALSE;
  METHODS
    wake();
    subdivide();
  END;
  
  LWatcher = PWatcher OBJECT
    cube : MeshNodeList.T;
  OVERRIDES
    wake := WakeLWatcher;
    subdivide := LSubdivide;
  END;

  (* representation of a coordinate cube:

     the binary index into the list of points is the combination of
     coordinates

     i.e., index 0 is the "bottom right" corner 
           index 2^N-1 is the "top left" corner

     each individual dimension delta can be calculated from just
     these two points 
  *)

PROCEDURE InitMN(mn : MeshNode.T) : MeshNode.T =
  BEGIN
    mn.id := nextJobId;
    INC(nextJobId);
    RETURN mn
  END InitMN;

PROCEDURE Run(s : RefSeq.T) = 
  VAR
    n   := s.size();
    lim := NEW(REF Dims.T, n); (* lim     *)
    q   := Dims.Clone(lim^);   (* cur val *)
    c   := 0;
    missing : IntSet.T;
  BEGIN
    FOR i := FIRST(lim^) TO LAST(lim^) DO
      lim[i] := NARROW(s.get(i),Settings).n();
    END;

    WITH iter = Dims.Iterate(lim^) DO
      WHILE iter.next(q^) DO
        Debug.Out("iter " & Dims.Format(q^));
        IF ISTYPE(s.get(s.size()-1),Schmoo) THEN
          LaunchSchmoo(s, SUBARRAY(q^,0,n-1), s.get(n-1))
        ELSE
          
        END
      END
    END;

    WHILE activeNodes.size() # 0 DO

      (* lost lambs approach:
         1. see what's missing out of NB
         2. pick up any results
         3. see if anything is missing and didnt yield results -- those
            are the real lost lambs! 
      *)

      missing := NIL;

      IF c = 100 THEN
        TRY
          missing := SearchForLostLambs();
          c := 0
        EXCEPT
          ProcUtils.ErrorExit(x) =>
          Debug.Warning("SearchForLostLambs raised ErrorExit : " &
            Debug.UnNil(x.error))
        |
          ProcUtils.Timeout =>
          Debug.Warning("SearchForLostLambs raised Timeout")
        |
          Rd.Failure(x) =>
          Debug.Warning("SearchForLostLambs raised Rd.Failure : " &
            AL.Format(x))
        |
          OSError.E(x) =>
          Debug.Warning("SearchForLostLambs raised OSError.E : " &
            AL.Format(x))
        |
          Parse =>
          Debug.Warning("SearchForLostLambs couldnt parse output")
        END
      ELSE
        INC(c)
      END;

      (* missing may be NIL *)

      TRY
        IF NOT SearchForResults(missing) THEN
          Thread.Pause(0.10d0);
        END
      EXCEPT
        OSError.E(x) => Debug.Error("Schmoozer.Run SearchForResults : OSError.E : " &
          AL.Format(x))
      |
        Rd.Failure(x) => Debug.Error("Schmoozer.Run SearchForResults : Rd.Failure : " &
          AL.Format(x))
      |
        Parse => Debug.Error("Schmoozer.Run SearchForResults : parse error")
      END;

      IF missing # NIL THEN
        Debug.Out("Schmoozer.Run : missing.size() = " & Int(missing.size()));
        RelaunchLostLambs(missing)
      END
      
    END

  END Run;

PROCEDURE RelaunchLostLambs(lambs : IntSet.T) =
  VAR
    iter := lambs.iterate();
    nbId : INTEGER;
    nbCmd : NbCommand.T;
  BEGIN
    WHILE iter.next(nbId) DO
      IF FindJobByNbId(nbId, nbCmd) THEN
        LOCK nbMu DO
          nbCmd.nbId := -1;
          nbCmd.started := LAST(Time.T);
        END;
        Debug.Out("Re-enqueuing job " & Int(nbCmd.id));
        Enqueue(nbCmd)
      ELSE
        Debug.Warning(F("Cant find lamb %s ?????", Int(nbId)))
      END
    END
    
  END RelaunchLostLambs;

EXCEPTION Parse;
          
PROCEDURE SearchForLostLambs() : IntSet.T
  RAISES { ProcUtils.ErrorExit, ProcUtils.Timeout, Rd.Failure, OSError.E, Parse } =
  VAR
    cmd := "nbqstat user=" & user;
    dummy : CARDINAL;
    lambs := NEW(IntSetDef.T).init();
  BEGIN

    (* nbJobs contains running or waiting jobs *)
    (* search through jobs that ought to be running *)
    VAR
      id : CARDINAL;
      r : REFANY;
    BEGIN
      LOCK nbMu DO
        WITH iter = jobsById.iterate() DO
          WHILE iter.next(id, r) DO
            WITH nbc = NARROW(r, NbCommand.T) DO
              IF nbc.nbId # -1 THEN 
                Debug.Out("SearchForLostLambs: should be running: " & 
                  Int(nbc.nbId));
                EVAL lambs.insert(nbc.nbId)
              END
            END
          END
        END
      END
    END;

    WITH data = ProcUtils.ToText(cmd, timeout := NbTimeout),
         rd   = TextRd.New(data) DO
      TRY
        LOOP
          WITH line = Rd.GetLine(rd) DO
            IF TextUtils.FindSub(line, user, dummy) THEN
              (* match *)
              WITH rdr = NEW(TextReader.T).init(line),
                   <*UNUSED*>stateStr = rdr.nextE(" ", skipNulls := TRUE),
                   jobId    = Scan.Int(rdr.nextE(" ")) DO
                Debug.Out("SearchForLostLambs: found job " & Int(jobId), 20);
                EVAL lambs.delete(jobId)
              END
            END
          END
        END
      EXCEPT
        Rd.EndOfFile => (* skip *)
      |
        TextReader.NoMore, FloatMode.Trap, Lex.Error => RAISE Parse
      END
    END;

    Debug.Out(Int(lambs.size()) & " lamb(s) missing");
    VAR nbid : INTEGER;
        iter := lambs.iterate();
    BEGIN
      WHILE iter.next(nbid) DO
        Debug.Out("Missing lamb : " & Int(nbid))
      END
    END;

    RETURN lambs
  END SearchForLostLambs;

PROCEDURE SearchForResults(missing : IntSet.T) : BOOLEAN
  RAISES { OSError.E, Parse, Rd.Failure } =
  (* missing may be NIL *)

  PROCEDURE RemoveFromMissing() =
    BEGIN
      <*ASSERT missing # NIL*>
      LOCK nbMu DO
        VAR 
          iter := jobsById.iterate();
          id : CARDINAL;
          r : REFANY;
        BEGIN
          WHILE iter.next(id,r) DO
            WITH nbc = NARROW(r, NbCommand.T) DO
              IF foundSet.member(nbc.id) THEN
                Debug.Out(F("Found missing lamb %s %s", 
                            Int(nbc.id), Int(nbc.nbId)));
                EVAL missing.delete(nbc.nbId)
              END
            END
          END  
        END
      END
    END RemoveFromMissing;

  PROCEDURE MarkAsNotRunning() =
    VAR
      id : CARDINAL;
      iter := foundSet.iterate();
      r : REFANY;
    BEGIN
      WHILE iter.next(id) DO
        WITH hadIt = jobsById.get(id, r) DO
          IF hadIt THEN 
            LOCK nbMu DO
              WITH nbc = NARROW(r, NbCommand.T) DO
                nbc.nbId := -1;
                nbc.started := LAST(Time.T);
              END
            END
          ELSE
            Debug.Warning(F("Exited job %s wasn't running???", Int(id)))
          END
        END
      END
    END MarkAsNotRunning;

  PROCEDURE ProcessOutput(mn : MeshNode.T; ln : TEXT) RAISES { OSError.E } =
    BEGIN
      IF    TE(ln, "PASS") THEN
        mn.state := NodeState.Pass
      ELSIF TE(ln, "FAIL") THEN
        mn.state := NodeState.Fail
      ELSIF  TE(ln, "NOTYET") THEN
        mn.state := NodeState.ErrorExit
      ELSE
        Debug.Warning("Unknown result \"" & ln & "\", marking as error");
        mn.state := NodeState.ErrorExit
      END;
      ChangedState(mn);
      
      WITH wr = FileWr.Open(resultsDn & "/" & fn),
           sWr = pfWr[mn.state] DO
        
        Wr.PutText(r1Wr, fn); Wr.PutChar(r1Wr, ' ');

        FOR t := FIRST(Tool) TO LAST(Tool) DO
          FOR i := FIRST(mn.args^) TO LAST(mn.args^) DO
            WITH cmd = mn.args[i].format(t) DO
              IF Text.Length(cmd) # 0 THEN
                Wr.PutText(wr, cmd);
                Wr.PutChar(wr, '\n');
                
                Wr.PutText(r1Wr, cmd); Wr.PutChar(r1Wr, ' ');
                Wr.PutText(sWr, cmd); Wr.PutChar(sWr, ' ')
              END
            END
          END
        END;
        Wr.PutText(wr, "PassFail ");
        Wr.PutText(wr, NodeStateNames[mn.state]);
        Wr.PutChar(wr, '\n');
        Wr.Close(wr);
        
        Wr.PutText(r1Wr, NodeStateNames[mn.state]);
        Wr.PutChar(r1Wr, '\n');
        Wr.Flush(r1Wr);
        Wr.PutChar(sWr, '\n');
        Wr.Flush(sWr);
      END
    END ProcessOutput;
    
  VAR
    found := FALSE;
    iter  := FS.Iterate(doneDn);
    fn : Pathname.T;
    rd : Rd.T;
    r  : REFANY;
    foundSet := NEW(CardSetDef.T).init();
  BEGIN
    TRY
    WHILE iter.next(fn) DO
      found := TRUE;
      Debug.Out("Found \"" & fn & "\"");
      EVAL foundSet.insert(Scan.Int(fn));
      rd := FileRd.Open(rootDn & "/" & fn & "/result");
      WITH i     = Scan.Int(fn),
           ln    = Rd.GetLine(rd),
           hadIt = activeNodes.delete(i, r),
           mn    = NARROW(r, MeshNode.T) DO

        Debug.Out(F("activeNodes.delete(%s)",Int(i)));

        Debug.Out(F("%s %s", fn, ln));
        Rd.Close(rd);
        FS.DeleteFile(doneDn & "/" & fn);
        IF hadIt THEN
          ProcessOutput(mn, ln)
        ELSE
          Debug.Warning(F("Received result for unknown job %s", Int(i)))
        END
      END
    END
    EXCEPT
      FloatMode.Trap, Lex.Error, Rd.EndOfFile => RAISE Parse
    END;

    (* sequencing here is very tricky!!!! *)
    (* go through foundSet and remove from missing *)
    IF missing # NIL THEN
      RemoveFromMissing()
    END;

    (* now also ensure that the jobs are marked as not running anymore *)

    MarkAsNotRunning();

    RETURN found
  END SearchForResults;

PROCEDURE ChangedState(mn : MeshNode.T) =
  VAR
    p := mn.watchers;
  BEGIN
    Debug.Out(F("%s watchers", Int(RefList.Length(mn.watchers))));
    WHILE p # NIL DO
      NARROW(p.head,PWatcher).wake();
      p := p.tail
    END
  END ChangedState;

PROCEDURE LSubdivide(w : LWatcher) =

  VAR
    ll := w.cube.head;
    ur := MeshNodeList.Nth(w.cube, MeshNodeList.Length(w.cube)-1);
    ndims := ll.x.size();
    (* these two are indexed by dimension *)
    d := NEW(REF ARRAY OF LONGREAL, ndims);
    c := NEW(REF ARRAY OF LRSeq.T, ndims);
    split := FALSE;
    xc := NEW(REF ARRAY OF LONGREAL, ndims); (* centroid *)
  BEGIN
    Debug.Out(F("LSubdivide subdividing (%s , %s)", FormatPoint(ll.x), FormatPoint(ur.x)));
    
    FOR dim := 0 TO ndims-1 DO
      c[dim] := NEW(LRSeq.T).init()
    END;

    (* calculate the steps in each dimension and calculate the centroid 
       of the current voxel *)
    FOR dim := 0 TO ndims-1 DO
      d [dim] :=  FromList(ur.x)[dim] - FromList(ll.x)[dim]           ;
      xc[dim] := (FromList(ur.x)[dim] + FromList(ll.x)[dim]) / 2.0d0
    END;

    Debug.Out(F("LSubdivide delta    %s", FormatPoint(ToList(d^))));
    Debug.Out(F("LSubdivide centroid %s", FormatPoint(ToList(xc^))));
    
    FOR dim := 0 TO ndims-1 DO
      (* the boundary of the parent voxel is always included *)
      c[dim].addhi(ll.x.get(dim));

      (* if we are still allowed to split, then split in this dimension *)
      IF d[dim] > w.s.schmoo.minStep[dim] +
                  xc[dim] * w.s.schmoo.minRatio[dim]
       THEN
        split := TRUE;
        c[dim].addhi(xc[dim])
      END;

      (* and add the other boundary of the parent voxel *)
      c[dim].addhi(ur.x.get(dim));

      FOR i := 0 TO c[dim].size()-1 DO
        Debug.Out(F("Subdivide dim %s coord %s", Int(dim), LongReal(c[dim].get(i))))
      END
    END;

    IF split THEN
      (* at least one dimension has been split *)
      LaunchMesh(c^, w.s)
    ELSE
      Debug.Out("Achieved minstep, quitting here.");
      w.s.minSquares := XYList.Cons(ToList(xc^), w.s.minSquares)
    END
  END LSubdivide;

PROCEDURE LaunchMesh(READONLY c : ARRAY OF LRSeq.T; inst : SchmooInstance) =

  PROCEDURE Launch(ll, ur : Point.T;
                   d : CARDINAL) =
    (* recursively generate all the child voxels of the current voxel,
       using the steps in each dimension *)
    BEGIN
      IF d = ndims THEN
        LaunchList(inst, MakeCube(ll, ur))
      ELSE
        Debug.Out(F("Launch d %s c[d].size() %s",Int(d), Int(c[d].size())));
        
        FOR i := 0 TO c[d].size()-2 DO
          ll.put(d, c[d].get(i  ));
          ur.put(d, c[d].get(i+1));
          Debug.Out(F("Calling Launch( %s , %s )", FormatPoint(ll), FormatPoint(ur)));
          Launch(ll, ur, d+1)
        END
      END
    END Launch;

  VAR
    ndims := NUMBER(c);
  BEGIN
    WITH ll = EmptyPoint(), ur = EmptyPoint() DO
      FOR i := 0 TO ndims-1 DO
        ll.addhi(LAST(LONGREAL));
        ur.addhi(FIRST(LONGREAL))
      END;
      Launch(ll, ur, 0)
    END
  END LaunchMesh;
  
PROCEDURE EmptyPoint() : Point.T =
  BEGIN RETURN NEW(Point.T).init() END EmptyPoint;
  
PROCEDURE MakeCube(ll, ur : Point.T) : PointSeq.T =
  VAR
    d := ll.size();        (* dimensionality *)
    n := Word.Shift(1, d); (* size of cube *)
    next : Point.T;
    res := NEW(PointSeq.T).init();
  BEGIN
    Debug.Out(F("MakeCube ll=%s ur=%s", FormatPoint(ll), FormatPoint(ur)));
    FOR i := 0 TO n-1 DO
      next := EmptyPoint();
      FOR b := 0 TO d-1 DO
        WITH q = Word.Extract(i, b, 1) DO
          CASE q OF
            0 => next.addhi(ll.get(b))
          |
            1 => next.addhi(ur.get(b))
          ELSE
            <*ASSERT FALSE*>
          END
        END
      END(* FOR b *);
      Debug.Out("MakeCube next=" & FormatPoint(next));
      res.addhi(next)
    END;
    RETURN res
  END MakeCube;

PROCEDURE WakeLWatcher(w : LWatcher) =
  VAR
    count := ARRAY NodeState OF CARDINAL { 0, .. };
    p := w.cube;
  BEGIN
    IF w.done THEN RETURN END; (* crud *)
    Debug.Out("WakeLWatcher " & Int(w.id));
    w.done := TRUE;

    WHILE p # NIL DO
      INC(count[p.head.state]);
      p := p.tail
    END;

    IF    (count[NodeState.Pass] # 0 AND count[NodeState.Fail] # 0) OR
           count[NodeState.ErrorExit] # 0 THEN
      Debug.Out("Different states!");
      w.subdivide();
      w.done := TRUE
    ELSIF count[NodeState.Running] # 0 THEN
      Debug.Out(F("WakeLWatcher: node(s) still running, going back to sleep"));
      w.done := FALSE
    ELSE
      WITH len = MeshNodeList.Length(w.cube) DO
        <*ASSERT count[NodeState.Pass] = len OR count[NodeState.Fail] = len *>
      END;
      Debug.Out("All states the same, nothing to do here");
      w.done := TRUE
    END
  END WakeLWatcher;

PROCEDURE LaunchSchmoo(pfx : RefSeq.T; READONLY q : Dims.T; schmoo : Schmoo) =
  (* note we can launch many schmoos *)
  VAR
    ndims := NUMBER(schmoo.param^);
    valPfx := NEW(REF ARRAY OF Val, NUMBER(q));
    
    actStep := NEW(REF ARRAY OF LONGREAL, ndims);
    inst : SchmooInstance;
    c := NEW(REF ARRAY OF LRSeq.T, ndims);
    split := FALSE;
  BEGIN
    FOR i := FIRST(valPfx^) TO LAST(valPfx^) DO
      WITH s = NARROW(pfx.get(i),Settings) DO
        TYPECASE s OF
          Variety, Sweep, SweepSpecific =>
          valPfx[i] := NEW(StepVal, i := q[i])
        |
          Schmoo => valPfx[i] := NEW(SchmooVal)
        ELSE
          <*ASSERT FALSE*>
        END
      END;
      valPfx[i].s := pfx.get(i)
    END;

    FOR dim := 0 TO ndims-1 DO
      c[dim] := NEW(LRSeq.T).init()
    END;

    inst := NEW(SchmooInstance, 
                schmoo     := schmoo, 
                valPfx     := valPfx,
                meshTab    := NEW(PointRefTbl.Default).init());
    
    schmooInstances := RefList.Cons(inst, schmooInstances);

    <*ASSERT NUMBER(q) = pfx.size()-1*>
    FOR i := 0 TO ndims-1 DO
      IF schmoo.max[i] = schmoo.min[i] THEN
        actStep[i] := 0.0d0
      ELSE
        split := TRUE;
        WITH dd = CEILING((schmoo.max[i]-schmoo.min[i])/schmoo.maxStep[i])+1 DO
          actStep[i] := (schmoo.max[i]-schmoo.min[i])/FLOAT(dd-1,LONGREAL)
        END
      END;

      (* generate all the coordinate points for each dimension *)
      VAR
        p := schmoo.min[i];
      BEGIN
        (* careful with roundoff here.

           final p should really be schmoo.max[i] but with roundoff it
           might be a bit smaller (and if looked for c.gethi() = schmoo.max,
           the program would likely never terminate
        *)
        REPEAT
          c[i].addhi(p);
          p := p + actStep[i]
        UNTIL c[i].gethi() > schmoo.max[i] - actStep[i]/2.0d0;
      END
    END;

    IF split THEN
      LaunchMesh(c^, inst)
    ELSE
      <* ASSERT FALSE *> (* this worked before but semantics were screwed up *)
    END
  END LaunchSchmoo;

 

PROCEDURE ToList(READONLY x : ARRAY OF LR) : Point.T =
  VAR res := EmptyPoint();
  BEGIN
    FOR i := FIRST(x) TO LAST(x) DO
      res.addhi(x[i])
    END;
    RETURN res
  END ToList;

PROCEDURE FromList(l : Point.T) : REF LA =
  VAR
    res := NEW(REF LA, l.size());
  BEGIN
    FOR i := 0 TO l.size()-1 DO
      res[i] := l.get(i)
    END;
    RETURN res
  END FromList;

VAR
  nextWatcherId := 0;

PROCEDURE FormatPoint(p : Point.T) : TEXT =
  VAR res := "";
  BEGIN
    FOR i := 0 TO p.size()-1 DO
      res := res & " " & LongReal(p.get(i))
    END;
    RETURN res
  END FormatPoint;
  
PROCEDURE LaunchList(inst : SchmooInstance; p : PointSeq.T) =
  VAR
    new := NEW(RefSeq.T).init();
    m : MeshNodeList.T;
    r : REFANY;
  BEGIN
    FOR i := 0 TO p.size()-1 DO
      Debug.Out("LaunchList " & FormatPoint(p.get(i)))
    END;
    
    FOR i := 0 TO p.size()-1 DO
      WITH pp = p.get(i) DO
        IF inst.meshTab.get(pp, r) THEN
          (* it already exists, link to old run *)
          m := MeshNodeList.Cons(r, m)
        ELSE
          m := MeshNodeList.Cons(NEW(MeshNode.T, x := pp).init(), m);
          EVAL inst.meshTab.put(pp, m.head);
          new.addhi(m.head)
        END
      END
    END;

    m := MeshNodeList.ReverseD(m);
    
    VAR
      w := NEW(LWatcher, id := nextWatcherId, s := inst, cube := m);
      mp := m;
    BEGIN
      INC(nextWatcherId);
      WHILE mp # NIL DO
        Debug.Out(F("Adding watcher %s to cmd %s ", 
                    Int(w.id), Int(mp.head.id)));
        mp.head.watchers := RefList.Cons(w, mp.head.watchers);
        mp := mp.tail
      END
    END;
    
    (* actually launch new jobs *)
    FOR i := 0 TO new.size()-1 DO
      WITH m       = NARROW(new.get(i),MeshNode.T),
           an      = NUMBER(inst.valPfx^),
           allVals = NEW(REF ARRAY OF Val, an) DO
        SUBARRAY(allVals^, 0, NUMBER(inst.valPfx^)) := inst.valPfx^;
        allVals[an-1] := NEW(SchmooVal, 
                             s := inst.schmoo, 
                             x := m.x);
        
        LaunchSingleJob(allVals^, m.id);
        m.args := allVals;
        Debug.Out(F("activeNodes.put(%s)",Int(m.id)));
        EVAL activeNodes.put(m.id, m)
      END
    END
  END LaunchList;
  
VAR
  nextJobId := 0;
  activeNodes := NEW(CardRefTbl.Default).init();

CONST NetBatchString = "nbq";

CONST FNFmt = "%08s";

PROCEDURE FindSim(READONLY v : ARRAY OF Val) : TEXT =
  BEGIN
    FOR i := FIRST(v) TO LAST(v) DO
      TYPECASE v[i] OF 
        StepVal(st) =>
        TYPECASE st.s OF 
          Variety(vv) =>
          IF vv.param = simP THEN
            RETURN vv.cover[st.i]
          END
        ELSE (* skip *) END
      ELSE (* skip *) END
    END;
    <*ASSERT FALSE*>
  END FindSim;

PROCEDURE FormatCommand(id : CARDINAL;
                        pfx : TEXT;
                        READONLY v : ARRAY OF Val;
                        memoize : BOOLEAN;
                        VAR didMemoize : BOOLEAN) : TEXT =
  VAR
    command := pfx;
    beforeSpice := command;
    afterSpice := "";
    memoId : CARDINAL;
    
  BEGIN
    didMemoize := FALSE;
    FOR t := FIRST(Tool) TO LAST(Tool) DO
      FOR i := FIRST(v) TO LAST(v) DO
        <*ASSERT v[i] # NIL*>
        WITH fm = v[i].format(t) DO
          IF Text.Length(fm) # 0 THEN
            command := command & " " & fm;
            IF t = Tool.PostSpice THEN
              afterSpice := afterSpice & " " & fm
            ELSE
              beforeSpice := beforeSpice & " " & fm
            END
          END
        END
      END
    END;

    IF memoize THEN
      Debug.Out("FormatCommand seeking memo string : " & beforeSpice);
      
      IF memoCmds.get(beforeSpice, memoId) THEN
        Debug.Out("FormatCommand found old command " & Int(memoId) & " -> " & Int(id));
        command := F(" -memoize " & FNFmt & " %s", Int(memoId), command);
        didMemoize := TRUE
      ELSE
        Debug.Out("FormatCommand remembering new command");
        EVAL memoCmds.put(beforeSpice, id)
      END
    END;

    RETURN command
  END FormatCommand;

VAR memoCmds := NEW(TextCardTbl.Default).init();
  
PROCEDURE LaunchSingleJob(READONLY v : ARRAY OF Val; id : CARDINAL) =
  VAR
    jdn := F("%s/" & FNFmt,absRoot,Int(id));
    nbs := F("%s --log-file-dir %s", NetBatchString, rootDn);
    command1 := F("%s %s %s " & FNFmt & " ", 
                  nbs, cmdNm, absRoot, Int(id));
    command2 := FindSim(v) & " ";
    command : TEXT;
    didMemoize : BOOLEAN;
  BEGIN
    TRY
      FS.CreateDirectory(jdn)
    EXCEPT
      OSError.E(x) =>
      Debug.Error("Couldnt create directory \""& jdn & "\" : OSError.E : " &
        AL.Format(x))
    END;

    command := command1 & FormatCommand(id, command2, v, memoize := TRUE,
                                        didMemoize := didMemoize);

    Debug.Out(F("command %s : %s", Int(id), command));
    Wr.PutText(clogWr, F("%s %s\n", Int(id), command));
    Wr.Flush  (clogWr);
    VAR nbCmd := NEW(NbCommand.T, id := id, cmd := command);
        pri := FIRST(Pri);
    BEGIN
      IF didMemoize THEN pri := LAST(Pri) END;
     
      Enqueue(nbCmd, pri);
      LOCK nbMu DO
        EVAL jobsById.put(id, nbCmd)
      END
    END
  END LaunchSingleJob;

CONST NPri = 2;
TYPE Pri = [0..NPri-1];
VAR cmdQ  : ARRAY Pri OF NbCommandSeq.T;
VAR cmdMu := NEW(MUTEX);
VAR cmdC  := NEW(Thread.Condition);

PROCEDURE Enqueue(command : NbCommand.T; pri := FIRST(Pri)) =
  BEGIN
    Debug.Out("Enqueue command with pri " & Int(pri));
    LOCK cmdMu DO cmdQ[pri].addhi(command); Thread.Signal(cmdC) END;
    Debug.Out("Enqueue command done");
  END Enqueue;

PROCEDURE Dequeue() : NbCommand.T =

  PROCEDURE Size() : CARDINAL =
    VAR res := 0;
    BEGIN
      FOR i := FIRST(Pri) TO LAST(Pri) DO
        res := res + cmdQ[i].size()
      END;
      RETURN res
    END Size;
    
  BEGIN
    LOCK cmdMu DO
      Debug.Out("Dequeue : " & Int(Size()) & " commands waiting to run");
      FOR i := FIRST(Pri) TO LAST(Pri) DO
        Debug.Out(F("Dequeue cmdQ[%s] size %s", Int(i), Int(cmdQ[i].size())))
      END;
      WHILE Size() = 0 DO Thread.Wait(cmdMu, cmdC) END;
      (* \exists i s.t.  cmdQ[i].size() > 0 *)
      FOR i := FIRST(Pri) TO LAST(Pri) DO
        IF cmdQ[i].size() # 0 THEN
          Debug.Out("Dequeue command from queue " & Int(i));
          RETURN cmdQ[i].remlo()
        END
      END;
      <*ASSERT FALSE*>
    END
  END Dequeue;
  
VAR nbMu       := NEW(MUTEX);
VAR jobsById   := NEW(CardRefTbl.Default).init();

PROCEDURE FindJobByNbId(nbId : INTEGER; VAR nbCmd : NbCommand.T) : BOOLEAN =
  BEGIN
    IF nbId = -1 THEN
      Debug.Warning("Searching for NbCommand with NBID -1, can't exist!");
      RETURN FALSE
    END;
    LOCK nbMu DO
      VAR 
        iter := jobsById.iterate();
        id : CARDINAL;
        r : REFANY;
      BEGIN
        WHILE iter.next(id, r) DO
          WITH cand = NARROW(r, NbCommand.T) DO
            IF cand.nbId = nbId THEN nbCmd := cand; RETURN TRUE END
          END
        END
      END
    END;
    RETURN FALSE
  END FindJobByNbId;

PROCEDURE IntAfter(in, word : TEXT) : INTEGER
  RAISES { Lex.Error, FloatMode.Trap } =
  TYPE
    SC = SET OF CHAR;
  VAR
    p : CARDINAL;
    n := Text.Length(in);
  BEGIN
    IF NOT TextUtils.FindSub(in, word, p) THEN RAISE Lex.Error END;
    
    p := p + Text.Length(word);

    WHILE p < n AND NOT Text.GetChar(in, p) IN SC { '-', '0'..'9' } DO
      INC(p)
    END;

    IF p = n THEN RAISE Lex.Error END;
    
    VAR s := p+1;
    BEGIN
      WHILE Text.GetChar(in, s) IN SC { '0'..'9' } DO INC(s) END;
      RETURN Scan.Int(Text.Sub(in, p, s-p))
    END
  END IntAfter;

PROCEDURE LaunchApply(<*UNUSED*>cl : Thread.Closure) : REFANY =
  VAR
    cmd : NbCommand.T;
  BEGIN
    LOOP(*forever*)

      WHILE NOT GetQuota() DO Thread.Pause(0.1d0) END;

      (* got quota *)

      cmd := Dequeue();

      Debug.Out(F("LaunchApply actually launching command: %s %s",
                  Int(cmd.id), cmd.cmd));

      VAR 
        success := FALSE;
        output : TEXT;
      CONST
        Attempts = 10;
      BEGIN
        FOR i := 1 TO Attempts DO
          TRY
            output := ProcUtils.ToText(cmd.cmd, timeout := NbTimeout);
            Debug.Out("LaunchApply command output : " & output);

            WITH nbId = IntAfter(output, "JobID") DO
              Debug.Out("LaunchApply NB id is " & Int(nbId));
              LOCK nbMu DO 
                cmd.nbId := nbId;
                cmd.started := Time.Now()
              END;
              Debug.Out("LaunchApply: nbMu released")
            END;

            success := TRUE; EXIT
          EXCEPT
            OSError.E, Rd.Failure =>
            Debug.Warning("LaunchApply : caught OSError or Rd.Failure");
            Thread.Pause(100.0d0)
            
          |
            FloatMode.Trap, Lex.Error =>
            Debug.Warning("LaunchApply : caught FloatMode.Trap or Lex.Error");
            Thread.Pause(100.0d0)
            
          |
            ProcUtils.ErrorExit(x) =>
            Debug.Warning("LaunchApply : caught error ErrorExit : " & ProcUtils.FormatError(x));
            Thread.Pause(10.0d0)
          |
            ProcUtils.Timeout =>
            Debug.Warning("LaunchApply : caught timeout");
            Thread.Pause(10.0d0)
          END
        END;
        IF NOT success THEN 
          Debug.Error("LaunchApply : command failed repeatedly : " & cmd.cmd) 
        END
      END

    END
  END LaunchApply;

CONST NLaunchers = 10;
      
PROCEDURE StartLaunchers() =
  BEGIN
    FOR i := 0 TO NLaunchers-1 DO
      EVAL Thread.Fork(NEW(Thread.Closure, apply := LaunchApply))
    END
  END StartLaunchers;
    
(**********************************************************************)

VAR q : CARDINAL := 0;
VAR qMu := NEW(MUTEX);
    
PROCEDURE QuotaApply(<*UNUSED*>cl : Thread.Closure) : REFANY =
  VAR
    tq : CARDINAL;
    delay : LONGREAL;

  CONST
    DelayIncr = 0.5d0;
    DelayMax  = 5.0d0;

  BEGIN
    LOOP
      (* wait until we run out of quota *)
      REPEAT
        LOCK qMu DO tq := q END;
        IF tq > 0 THEN Thread.Pause(0.1d0) END
      UNTIL tq = 0;

      delay := 0.5d0;

      (* theres a race condition here.
         launch threads can get quota, then we can find nbavail before
         jobs have been launched, overrunning our buffer *)
      
      WHILE tq = 0 DO
        tq := GetLaunchQuota();
        LOCK qMu DO q := tq END;
        IF tq = 0 THEN
          Thread.Pause(delay);
          delay := MIN(delay + DelayIncr, DelayMax)
        END
      END
    END
  END QuotaApply;

PROCEDURE GetQuota() : BOOLEAN =
  BEGIN
    LOCK qMu DO
      IF q > 0 THEN
        DEC(q);
        Debug.Out("GetQuota : rem. quota " & Int(q));
        RETURN TRUE
      ELSE
        RETURN FALSE
      END
    END
  END GetQuota;

(**********************************************************************)
  
PROCEDURE GetLaunchQuota() : CARDINAL =
  BEGIN 
    LOOP
      TRY
        Debug.Out("GetLaunchQuota: trying to run nbavail");
        WITH res = Scan.Int(ProcUtils.ToText("nbavail", timeout := NbTimeout))  DO
          Debug.Out("GetLaunchQuota: res = " & Int(res));
          RETURN res
        END
      EXCEPT
        OSError.E(x) => 
        Debug.Warning("Trouble running nbavail: OSError.E " & AL.Format(x))
      |
        Rd.Failure(x) => 
        Debug.Warning("Trouble running nbavail: Rd.Failure " & AL.Format(x))
      |
        ProcUtils.ErrorExit(x) => 
        Debug.Warning("Trouble running nbavail: " & ProcUtils.FormatError(x));
      |
        ProcUtils.Timeout => 
        Debug.Warning("Trouble running nbavail: timeout")
      |
        Lex.Error, FloatMode.Trap =>        
        Debug.Warning("Trouble parsing output of nbavail...")
      END;
      Thread.Pause(10.0d0)
    END
  END GetLaunchQuota;

PROCEDURE Add(s : Settings) =
  BEGIN
    (* invariants:
       1. at most one Schmoo.
       2. Schmoo always at the end *)
    IF ISTYPE(s, Schmoo) THEN
      <*ASSERT settings.size()=0 OR 
               NOT ISTYPE(settings.get(settings.size()-1),Schmoo) *>
      settings.addhi(s)
    ELSE
      settings.addlo(s)
    END
  END Add;
  
PROCEDURE Process(READONLY a : ARRAY OF Schmooze.T) =
  BEGIN
    IF pp.keywordPresent ("-schmooze") THEN
      WITH snm = pp.getNext() DO
        FOR i := FIRST(a) TO LAST(a) DO
          IF TE(a[i].nm,  snm) THEN
            a[i].p(); RETURN
          END
        END;
        Debug.Error("unknown schmooze \"" & snm & "\"")
      END
    ELSE
      a[0].p()
    END
  END Process;

PROCEDURE Setup(simPA : DiscreteParam) =
  BEGIN
    simP := simPA;
    FOR i := FIRST(Pri) TO LAST(Pri) DO
      cmdQ[i] :=   NEW(NbCommandSeq.T).init()
    END;

    EVAL Thread.Fork(NEW(Thread.Closure, apply := QuotaApply));

    StartLaunchers();

    IF pp.keywordPresent("-dn") THEN
      rootDn := pp.getNext()
    END;
    
    doneDn := rootDn & "/done";
    resultsDn := rootDn & "/results";
    resultsFn := rootDn & "/results.dat"; (* single file *)
    clogFn    := rootDn & "/commands.dat";
    
    <*FATAL OSError.E*>
    BEGIN
      FS.CreateDirectory(rootDn); (* source of everything *)
      FS.CreateDirectory(doneDn);
      FS.CreateDirectory(resultsDn);
      r1Wr := FileWr.Open(resultsFn);
      
      clogWr := FileWr.Open(clogFn);
      
      FOR i := FIRST(pfWr) TO LAST(pfWr) DO
        pfWr[i] := FileWr.Open(rootDn & 
                       "/" & 
                       TextUtils.ToLower(NodeStateNames[i]) & 
                       ".dat")
      END;
      
      absRoot := FS.GetAbsolutePathname(rootDn);
    END;
  END Setup;

PROCEDURE RunAll() =
  BEGIN

  pp.finish();
  
  Run(settings);
  Wr.Close(r1Wr);
  FOR i := FIRST(pfWr) TO LAST(pfWr) DO
    Wr.Close(pfWr[i])
  END;

  (* dump minSquares *)
  VAR
    p := schmooInstances;
    fn : Pathname.T;
    wr : Wr.T;
  BEGIN
    WHILE p # NIL DO
      WITH si = NARROW(p.head, SchmooInstance) DO
        fn := "";
        FOR t := FIRST(Tool) TO LAST(Tool) DO
          FOR i := FIRST(si.valPfx^) TO LAST(si.valPfx^) DO
            WITH cmd = si.valPfx[i].format(t) DO fn := fn & cmd END
          END
        END;

        fn := TextUtils.Replace(fn, " ", "__");
        fn := TextUtils.Replace(fn, "-", "_");

        wr := FileWr.Open(rootDn & "/" & fn & "_boundary.dat");
        VAR q := si.minSquares; BEGIN
          WHILE q # NIL DO
            Wr.PutText(wr, FormatPoint(q.head));
            Wr.PutChar(wr, '\n');
            q := q.tail
          END
        END;
        Wr.Close(wr)
      END;
      p := p.tail
    END
  END;

  Wr.Close(clogWr)
  END RunAll;
  
VAR
  now      := Time.Now();
  nowD     := Date.FromTime(now);
  settings := NEW(RefSeq.T).init();
  frac     := now-FLOAT(TRUNC(now),Time.T);
  rootDn   := F("schmoozer%04s-%02s-%02s@", 
                Int(nowD.year), Int(ORD(nowD.month)+1), Int(nowD.day)) &
              F("%02s:%02s:%02s.%03s",
                Int(nowD.hour), Int(nowD.minute), Int(nowD.second), 
                Int(TRUNC(1000.0d0*frac)));

  cmdNm    := "runspice.sh";
  absRoot : Pathname.T;
  doneDn, resultsDn, resultsFn : Pathname.T;
  clogFn  : Pathname.T;
  clogWr  : Wr.T;

  r1Wr : Wr.T;
  pfWr : ARRAY [NodeState.Fail .. NodeState.ErrorExit] OF Wr.T;
  pp := NEW(ParseParams.T).init(Stdio.stderr);

  user := Env.Get("USER");

BEGIN END Schmoozer.

  


  
