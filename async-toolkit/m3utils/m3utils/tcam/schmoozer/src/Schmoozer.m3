MODULE Schmoozer;
IMPORT Schmooze;
IMPORT RefSeq;
IMPORT Dims;
IMPORT Debug;
IMPORT RefList;
IMPORT FS;
IMPORT Time, Date;
FROM Fmt IMPORT Int, LongReal, F, Style;
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
IMPORT LRVector, Matrix, LRScalarField;
IMPORT Powell;
IMPORT Wx;
IMPORT ThreadList;
IMPORT Math;
IMPORT SchmoozerResult; FROM SchmoozerResult IMPORT Parse;
IMPORT TextRefTbl;
IMPORT NewUOA_M3, NewUOAs;
FROM LRMatrix2 IMPORT NewV, V; IMPORT LRMatrix2;
IMPORT ConstrainedSpace;
IMPORT ThreadF;

<*FATAL Thread.Alerted*>
<*FATAL Wr.Failure*>

CONST TE = Text.Equal;
CONST NbTimeout = 60.0d0; (* timeout for all NB commands *)
CONST Eps = 1.0d-8;

CONST Verbose = FALSE;

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

PROCEDURE RI(READONLY z : IA) : REF IA =
  VAR
    res := NEW(REF IA, NUMBER(z));
  BEGIN
    res^ := z;
    RETURN res
  END RI;

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

  SpecificInt =  SingleSettings OBJECT
    v : REF ARRAY OF INTEGER; END BRANDED OBJECT
  OVERRIDES 
    n := NSpecificInt;
  END;

  Schmoo =  PublicSchmoo BRANDED OBJECT
  OVERRIDES
    n := NSchmoo;
  END;

  VarOpt = PublicVarOpt BRANDED OBJECT
  OVERRIDES
    n := NSchmoo;
  END;

CONST ToolNames = ARRAY Tool OF TEXT { "script", "spicebuilder", "postspice" };
      
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

  VarOptVal = Val OBJECT x : LRVector.T; argList : REF ARRAY OF TEXT END;
  
VAR schmooInstances : RefList.T := NIL;

PROCEDURE LongRealVal(v : Val) : LONGREAL =
  VAR 
    str : TEXT;
  BEGIN
    TYPECASE v OF  (* ugly *)
      StepVal(st) =>

      TYPECASE st.s OF 
        Variety(v) => 
        <*ASSERT v # NIL*>
        <*ASSERT st # NIL*>
        str := v.cover[st.i];
        RETURN Scan.LongReal(str)
      |
        Sweep(w) => 
        WITH lr = MIN(w.min + FLOAT(st.i,LR)*w.step, w.max) DO
          RETURN lr
        END
      |
        SweepSpecific(w) => 
        WITH lr = w.v[st.i] DO
          RETURN lr
        END
      |
        SpecificInt(w) => 
        WITH int = w.v[st.i] DO
          str := Int(int, base := NARROW(w.param,IntParam).base);
          RETURN Scan.LongReal(str)
        END
      ELSE
        <*ASSERT FALSE*>
      END(*TYPECASE st.s*);
    END
  END LongRealVal;

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
      |
        SpecificInt(w) => 
        WITH int = w.v[st.i] DO
          str := Int(int, base := NARROW(w.param,IntParam).base)
        END
      ELSE
        <*ASSERT FALSE*>
      END(*TYPECASE st.s*);
      
      WITH ss = NARROW(st.s,SingleSettings) DO
        IF Verbose THEN Debug.Out("FormatV scanning param " & ss.param.nm) END;
        VAR res : TEXT; BEGIN
          IF tool = ss.param.tool THEN
            res := F("-%s %s", ss.param.flag, str)
          ELSE
            res := ""
          END;
          IF Verbose THEN Debug.Out("FormatV returning " & res) END;
          RETURN res
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
    |
      VarOptVal(vv) =>
      VAR
        str := "";
      BEGIN
        IF tool = Tool.SpiceBuilder THEN
          str := F("-var %s", NARROW(vv.s,VarOpt).varName);
          FOR i := FIRST(vv.x^) TO LAST(vv.x^) DO
            str := str & F(" %s %s",vv.argList[i],LongReal(vv.x[i]))
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

PROCEDURE NSpecificInt(s : SpecificInt) : CARDINAL =
  BEGIN RETURN NUMBER(s.v^) END NSpecificInt;

PROCEDURE NSchmoo(<*UNUSED*>s : Settings) : CARDINAL = BEGIN RETURN 1 END NSchmoo;

REVEAL
  MeshNode.T = SchmoozerResult.T BRANDED OBJECT
    id    : CARDINAL;
    x     : Point.T;
    state := NodeState.Running; (* ?? *)
    watchers : RefList.T := NIL;
    args  : REF ARRAY OF Val;
  METHODS
    init() : MeshNode.T := InitMN;
  OVERRIDES
    processOutput := ProcessOutputMN;
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
    iter : Dims.Iterator;
  BEGIN
    FOR i := FIRST(lim^) TO LAST(lim^) DO
      lim[i] := NARROW(s.get(i),Settings).n();
    END;

    iter := Dims.Iterate(lim^);

    TYPECASE s.get(n-1) OF
      Schmoo(schmoo) =>
      WHILE iter.next(q^) DO
        IF Verbose THEN Debug.Out("iter " & Dims.Format(q^)) END;
        LaunchSchmoo(s, SUBARRAY(q^,0,n-1), schmoo)
      END;

      LOOP
        (* careful about races here.  
           since this is really only a single thread, the lock is a no-op 
           
           else we would have to worry about a race between the other thread's
           deleting objects and ours adding new ones.  There could be a gap
           with an empty activeNodes and thus a spurious program exit.
        *)
        LOCK activeMu DO
          IF activeNodes.size() = 0 THEN EXIT END
        END;
        RelaunchMissing(c)
      END
    |
      VarOpt(varopt) =>
      
      WHILE iter.next(q^) DO
        FOR i := FIRST(varopt.radius^) TO LAST(varopt.radius^) DO
          WITH r = varopt.radius[i] DO
            Debug.Out(F("launching varopt iter %s radius %s", Dims.Format(q^), LongReal(r)));
            AddThread(LaunchVarOpt(s, r, SUBARRAY(q^,0,n-1), varopt))
          END
        END
      END;

      LOOP
        LOCK activeMu DO VAR as := activeNodes.size(); BEGIN
          IF as = 0 AND NOT HaveThreads() THEN EXIT END
        END END;
        RelaunchMissing(c)
      END
    ELSE
      Debug.Error("Unsupported schmoozer type")
    END
  END Run;

VAR minThrs : ThreadList.T := NIL;
    minMu   := NEW(MUTEX);

PROCEDURE DeleteThread(thr : Thread.T) =
  VAR
    skipped := FALSE;
  BEGIN
    LOCK minMu DO
      VAR new : ThreadList.T := NIL;
          p := minThrs;
      BEGIN
        WHILE p # NIL DO
          IF p.head = thr THEN
            skipped := TRUE
          ELSE
            new := ThreadList.Cons(p.head, new)
          END;
          p := p.tail
        END;
        <*ASSERT skipped*>
        minThrs := new
      END
    END
  END DeleteThread;

PROCEDURE AddThread(thr : Thread.T) =
  BEGIN
    LOCK minMu DO
      minThrs := ThreadList.Cons(thr, minThrs)
    END
  END AddThread;

PROCEDURE HaveThreads() : BOOLEAN =
  BEGIN
    LOCK minMu DO
      RETURN minThrs # NIL
    END
  END HaveThreads;

PROCEDURE RelaunchMissing(VAR c : INTEGER) =
  VAR
    missing : IntSet.T;
  BEGIN
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
        Thread.Pause(0.10d0)
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
  END RelaunchMissing;
  
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

PROCEDURE SearchForLostLambs() : IntSet.T
  RAISES { ProcUtils.ErrorExit, ProcUtils.Timeout, Rd.Failure, OSError.E,Parse } =
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

PROCEDURE ProcessOutputMN(mn : MeshNode.T; fn : Pathname.T)
  RAISES { OSError.E, Rd.Failure, Parse } =
  VAR
    rd : Rd.T;
    ln : TEXT;
  BEGIN
    TRY
      rd := FileRd.Open(rootDn & "/" & fn & "/result");
      ln := Rd.GetLine(rd);
    EXCEPT
      Rd.EndOfFile => RAISE Parse
    END;
    Debug.Out(F("%s %s", fn, ln));
    Rd.Close(rd);
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
  END ProcessOutputMN;

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
    
  VAR
    found := FALSE;
    iter  := FS.Iterate(doneDn);
    fn : Pathname.T;
    r  : REFANY;
    foundSet := NEW(CardSetDef.T).init();
    fi : INTEGER;
    resultCb : SchmoozerResult.T;
    hadIt : BOOLEAN;
  BEGIN
    WHILE iter.next(fn) DO
      found := TRUE;
      Debug.Out("Found \"" & fn & "\"");
      TRY
        fi := Scan.Int(fn)
      EXCEPT
        FloatMode.Trap, Lex.Error => RAISE Parse
      END;
      EVAL foundSet.insert(fi);

      LOCK activeMu DO hadIt := activeNodes.delete(fi, r) END;
      resultCb := r;

      Debug.Out(F("activeNodes.delete(%s)",Int(fi)));

      FS.DeleteFile(doneDn & "/" & fn);
      IF hadIt THEN
        resultCb.processOutput(fn)
      ELSE
        Debug.Warning(F("Received result for unknown job %s", Int(fi)))
      END
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

TYPE
  VarOptNode = SchmoozerResult.T BRANDED OBJECT
    haveVal := FALSE;
    validResult : BOOLEAN;
    val : LONGREAL;
    mu : MUTEX;
    c  : Thread.Condition;
  OVERRIDES
    processOutput := ProcessOutputVO;
  END;

PROCEDURE ProcessOutputVO(n : VarOptNode; dirName : Pathname.T)
  RAISES { OSError.E, Rd.Failure, Parse } =
  VAR
    rd : Rd.T;
    ln : TEXT;
    pn := rootDn & "/" & dirName & "/asserter.measure";
    tr : TextReader.T;
  BEGIN
    Debug.Out(F("ProcessOutputVO %s", dirName));
    Debug.Out(F("ProcessOutputVO to open %s", pn));
    LOCK n.mu DO
      n.validResult := FALSE;
      TRY
        rd := FileRd.Open(pn);
        ln := Rd.GetLine(rd);
        tr := NEW(TextReader.T).init(ln);
        TRY
          WITH v  = tr.getLR() DO
            Debug.Out(F("ProcessOutputVO result %s", LongReal(v)));
            n.val := v;
            n.validResult := TRUE;
            n.haveVal := TRUE
          END
        EXCEPT
          FloatMode.Trap, Lex.Error =>
          Debug.Out(F("ProcessOutputVO result Not a leading number! %s", ln));
          n.haveVal := TRUE
        END
      EXCEPT
        TextReader.NoMore, Rd.EndOfFile =>
        Thread.Broadcast(n.c);
        RAISE Parse
      END
    END;
    Thread.Broadcast(n.c);
    Debug.Out(F("%s %s", dirName, ln));
    Rd.Close(rd);
  END ProcessOutputVO;
  
PROCEDURE LaunchVarOpt(pfx        : RefSeq.T;
                       radius     : LONGREAL;
                       READONLY q : Dims.T;
                       varopt     : VarOpt) : Thread.T =
  (* note we can launch many varopts *)
  VAR
    valPfx := NEW(REF ARRAY OF Val, NUMBER(q)-1);
  BEGIN
    Debug.Out("LaunchVarOpt()");
    FOR i := FIRST(valPfx^) TO LAST(valPfx^) DO
      WITH s = NARROW(pfx.get(i),Settings) DO
        TYPECASE s OF
          SingleSettings(ss) =>
          IF Verbose THEN Debug.Out(F("LaunchVarOpt i %s idx %s into settings %s", Int(i), Int(q[i]), ss.param.nm)) END;
          valPfx[i] := NEW(StepVal, i := q[i])
        ELSE
          <*ASSERT FALSE*>
        END
      END;
      valPfx[i].s := pfx.get(i)
    END;
    IF varopt.optMult = LAST(LONGREAL) THEN
      Debug.Error("Please dont forget to set optMult.  -1 for maximizing, +1 for minimizing.  Exiting...")
    END;
    RETURN Thread.Fork(NEW(VarOptThread,
                           radius := radius,
                           varopt := varopt,
                           valPfx := valPfx))
  END LaunchVarOpt;

TYPE
  VarOptThread = Thread.Closure OBJECT
    radius : LONGREAL;
    varopt : VarOpt;
    valPfx : REF ARRAY OF Val;
    mainDone   := FALSE; (* protected by varopt.watchedFunc.mu ! *)
    argList : REF ARRAY OF TEXT;
  OVERRIDES
    apply := OptApply;
  END;

PROCEDURE OptApply(thr : VarOptThread) : REFANY =
  <*FATAL Matrix.NotSquare*>
  CONST
    xtol = 1.0d-5;
  VAR
    alst := thr.varopt.dims.dims(thr.varopt.varName, "tttt");
    ndims := NUMBER(alst^);
    p := NEW(LRVector.T, ndims);
    res : LONGREAL;
    func := NEW(VarFunc,
                thr            := thr,
                maxThreads     := 1000,
                hintsByForking := TRUE,
                minMu          := NEW(MUTEX),
                minVal         := LAST(LONGREAL),
                minAt          := NEW(LRVector.T, ndims));
  CONST
    DoTest = FALSE;
  VAR
    testFunc := NEW(TestFuncType, thr := thr);
    cutoff := FIRST(LONGREAL);
  BEGIN
    thr.argList := alst;
    Debug.Out("OptApply ");
    FOR i := FIRST(thr.valPfx^) TO LAST(thr.valPfx^) DO
      IF ISTYPE(thr.valPfx^[i],StepVal) THEN
        WITH param =
             NARROW(NARROW(thr.valPfx^[i],StepVal).s,SingleSettings).param DO
          IF param = thr.varopt.stopParam THEN
            WITH val = LongRealVal(thr.valPfx^[i]) DO
              cutoff := thr.varopt.optMult * val * thr.varopt.stopRatio;
              Debug.Out("OptApply, cutoff is " & LongReal(cutoff));
              EXIT
            END
          END
        END
      END
    END;

    Debug.Out("cutoff " & LongReal(cutoff));
    VAR
      baseFunc : LRScalarField.T;
      watchedFunc : WatchedFunc;
      done : MinimizerThread := NIL;
    BEGIN
      IF DoTest THEN
        baseFunc := testFunc
      ELSE
        baseFunc := func
      END;
      watchedFunc := NEW(WatchedFunc).init(baseFunc);

      VAR
        mainCl := NEW(MainMinimizerCl,
                      p           := LRVector.Copy(p),
                      xtol        := xtol,
                      watchedFunc := watchedFunc,
                      radius      := thr.radius,
                      cutoff      := cutoff,
                      mu          := NEW(MUTEX),
                      parent      := thr,
                      parentId    := ThreadF.MyId());
        auxCl  : MinimizerThread;
        killAux : BOOLEAN;
        restartVal := LAST(LONGREAL);

      PROCEDURE StartAux() =
        BEGIN
          auxCl := NEW(AuxiliaryMinimizerCl,
                       parent      := thr,
                       mainWatched := watchedFunc,
                       baseFunc    := baseFunc,
                       watchedFunc := NEW(WatchedFunc).init(baseFunc),
                       xtol        := xtol,
                       cutoff      := cutoff,
                       mu          := NEW(MUTEX),
                       parentId    := ThreadF.MyId());
          EVAL Thread.Fork(auxCl);
        END StartAux;
      BEGIN

        Debug.Out("Starting aux minimizer");
        StartAux();
        Debug.Out("Starting main minimizer");
        EVAL Thread.Fork(mainCl);

        WHILE done = NIL DO
          Thread.Pause(0.1d0);

          (* check health of aux minimizer, it can break down if it 
             started wrong *)
          killAux := FALSE;
          IF Iterations(auxCl) >= 1 AND
             BestVal(mainCl) < BestVal(auxCl) AND
             BestVal(mainCl) < restartVal THEN
            killAux := TRUE
          END;

          IF killAux THEN
            Debug.Out("Killing aux thread.");
            KillMinimizer(auxCl);
            Debug.Out("Aux thread dead, creating new one");
            restartVal := BestVal(mainCl);
            StartAux()
          END;

          (* check for termination conditions *)
          IF Done(mainCl) AND
             Iterations(auxCl) >= 10 AND
             BestVal(mainCl) <= BestVal(auxCl) THEN
            (* main thread is done, ensure we have enough evals for 
               aux thread to have its say, best solution is apparently
               internal *)

            Debug.Out(F("Quit-case 1. BestVal(mainCl) = %s <= BestVal(auxCl) = %s", LongReal(BestVal(mainCl)), LongReal(BestVal(auxCl))));
            done := mainCl;
            KillMinimizer(auxCl);
            EXIT
          END;

          IF Done(auxCl) AND BestVal(auxCl) <= BestVal(mainCl) THEN
            (* aux thread is done, better than anything in main thread,
               declare that the answer *)
            Debug.Out(F("Quit-case 2. BestVal(mainCl) = %s >= BestVal(auxCl) = %s", LongReal(BestVal(mainCl)), LongReal(BestVal(auxCl))));
            done := auxCl;
            KillMinimizer(mainCl);
            EXIT
          END;

          IF Done(auxCl) AND
             Done(mainCl) AND
            BestVal(mainCl) <= BestVal(auxCl) THEN
            Debug.Out(F("Quit-case 3. BestVal(mainCl) = %s <= BestVal(auxCl) = %s", LongReal(BestVal(mainCl)), LongReal(BestVal(auxCl))));
            done := mainCl;
            EXIT
          END
        END;

        res := BestVal(done);
        p^  := BestX(done)^;

      END
    END;

    Debug.Out("Minimize returns " & LongReal(res));
    PushMinimum(res, thr.radius, thr.valPfx^, p);

    IF DoTest THEN Thread.Pause(0.0d0) END;
    DeleteThread(Thread.Self());
    RETURN NIL
  END OptApply;

PROCEDURE BestVal(cl : MinimizerThread) : LONGREAL =
  BEGIN
    LOCK cl.watchedFunc.mu DO
      VAR
        mr := cl.watchedFunc.bestF;
      BEGIN
        IF mr = 0.0d0/0.0d0 THEN mr := LAST(LONGREAL) END;
        <*ASSERT mr <= LAST(LONGREAL) AND mr >= FIRST(LONGREAL)*>
        RETURN mr
      END
    END
  END BestVal;
  
PROCEDURE BestX(cl : MinimizerThread) : LRVector.T =
  BEGIN
    LOCK cl.watchedFunc.mu DO RETURN cl.watchedFunc.bestX END
  END BestX;
  
PROCEDURE Iterations(cl : MinimizerThread) : CARDINAL =
  BEGIN
    LOCK cl.watchedFunc.mu DO RETURN cl.watchedFunc.cntF END
  END Iterations;
  
PROCEDURE Done(cl : MinimizerThread) : BOOLEAN =
  BEGIN
    LOCK cl.mu DO RETURN cl.res # LAST(LONGREAL) END
  END Done;
  
PROCEDURE KillMinimizer(cl : MinimizerThread) =
  BEGIN
    cl.watchedFunc.kill := TRUE;
    WHILE cl.res = LAST(LONGREAL) DO Thread.Pause(0.10d0) END;
  END KillMinimizer;

TYPE
  MinimizerThread = Thread.Closure OBJECT
    mu : MUTEX;
    res := LAST(LONGREAL);
    resV : LRVector.T;
    watchedFunc : WatchedFunc;
    parent       : VarOptThread; 
    parentId : CARDINAL;
  END;
  
  MainMinimizerCl = MinimizerThread OBJECT
    p : LRVector.T;
    xtol : LONGREAL;
    radius : LONGREAL;
    cutoff : LONGREAL;
  OVERRIDES
    apply := MMApply;
  END;

PROCEDURE MMApply(cl : MainMinimizerCl) : REFANY =
  BEGIN
    Debug.Out("MMApply, parent is " & Int(cl.parentId));
    WITH res = DoMinimize(cl.p, cl.xtol, cl.watchedFunc, cl.radius, cl.cutoff) DO
      Debug.Out("MMApply got res " & LongReal(res));
      LOCK cl.mu DO
        cl.res := res;
        cl.resV := LRVector.Copy(cl.p);
        Debug.Out(F("Main minimizer done res=%s @ %s ",
                    LongReal(cl.res),
                    LRMatrix2.FormatV(cl.resV^)))
      END;
      LOCK cl.watchedFunc.mu DO cl.parent.mainDone := TRUE END;
      Thread.Broadcast(cl.watchedFunc.c);
    END;
    RETURN NIL
  END MMApply;

TYPE
  AuxiliaryMinimizerCl = MinimizerThread OBJECT
    mainWatched  : WatchedFunc;     (* main thread func              *)
    baseFunc     : LRScalarField.T; (* base func, what to minimize   *)
    xtol, cutoff : LONGREAL;
  OVERRIDES
    apply := AuxMinApply;
  END;

PROCEDURE AuxMinApply(cl : AuxiliaryMinimizerCl) : REFANY =
  VAR
    dims := NUMBER(cl.parent.argList^);
   BEGIN
    Debug.Out("AuxMinApply, parent is " & Int(cl.parentId));
    Debug.Out("Auxiliary Minimizer dims=" & Int(dims));
    LOCK cl.mainWatched.mu DO
      WHILE cl.mainWatched.cntF < 2*dims AND NOT cl.parent.mainDone DO
        Debug.Out("cntF=" & Int(cl.mainWatched.cntF));
        Thread.Wait(cl.mainWatched.mu, cl.mainWatched.c)
      END
    END;

    (* there were at least 2*dims evals of the main minimizer and the
       best value so far is in watched *)
    VAR
      principal := NEW(REF V, dims);
      space : ConstrainedSpace.T;
      constrainedFunc : LRScalarField.T;
      p         := NEW(REF V, dims-1);
    BEGIN
      LOCK cl.mainWatched.mu DO
        principal^ := cl.mainWatched.bestX^
      END;
      Debug.Out("Auxiliary minimizer waking up, best point is " &
        LRMatrix2.FormatV(principal^));

      space := NEW(ConstrainedSpace.T).init(cl.parent.radius, principal^);

      (* note change of coordinates.
         watchedFunc is in the original coordinates
         constrainedFunc is in the reduced coordinates ON the constraint *)
      
      constrainedFunc := NEW(ConstrainedFunc,
                             cs   := space,
                             base := cl.watchedFunc);

      WITH res = DoMinimize(p,
                            cl.xtol,
                            constrainedFunc,
                            cl.parent.radius/10.0d0,
                            cl.cutoff) DO
        LOCK cl.mu DO
          cl.res := res;
          cl.resV := LRVector.Copy(p);
          Debug.Out(F("Aux minimizer done res=%s @ %s ",
                      LongReal(cl.res),
                      LRMatrix2.FormatV(cl.resV^)))
          
        END
      END
    END;
    RETURN NIL
  END AuxMinApply;

TYPE
  ConstrainedFunc = LRScalarField.T OBJECT
    cs : ConstrainedSpace.T;
    base : LRScalarField.T;
  OVERRIDES
    eval := EvalCF;
    evalHint := EvalHintCF;
  END;

PROCEDURE EvalCF(cf : ConstrainedFunc; at : LRVector.T) : LONGREAL =
  VAR
    coord := NEW(LRVector.T, NUMBER(at^)+1);
  BEGIN
    cf.cs.cons2cart(at^, coord^);
    RETURN cf.base.eval(coord)
  END EvalCF;

PROCEDURE EvalHintCF(cf : ConstrainedFunc; at : LRVector.T) =
  VAR
    coord := NEW(LRVector.T, NUMBER(at^)+1);
  BEGIN
    cf.cs.cons2cart(at^, coord^);
    cf.base.evalHint(coord)
  END EvalHintCF;
  

  (**********************************************************************)

TYPE
  WatchedFunc = LRScalarField.T OBJECT
    mu    : MUTEX;
    c     : Thread.Condition;
    f     : LRScalarField.T;
    bestF : LONGREAL;
    bestX : LRVector.T;
    cntF  : CARDINAL;
    kill  := FALSE;
  METHODS
    init(f : LRScalarField.T) : WatchedFunc := InitWF;
  OVERRIDES
    eval := EvalWF;
    evalHint := EvalHintWF;
  END;

PROCEDURE EvalWF(w : WatchedFunc; at : LRVector.T) : LONGREAL =
  BEGIN
    IF w.kill THEN RETURN 0.0d0/0.0d0 END;
    WITH res = w.f.eval(at) DO
      LOCK w.mu DO
        IF res < w.bestF THEN
          w.bestF := res;
          w.bestX := LRVector.Copy(at)
        END;
        INC(w.cntF)
      END;
      Thread.Broadcast(w.c);
      RETURN res
    END
  END EvalWF;

PROCEDURE EvalHintWF(w : WatchedFunc; at : LRVector.T) =
  BEGIN
    IF w.kill THEN RETURN END;
    w.f.evalHint(at)
  END EvalHintWF;

PROCEDURE InitWF(w : WatchedFunc; f : LRScalarField.T) : WatchedFunc =
  BEGIN
    w.mu    := NEW(MUTEX);
    w.c     := NEW(Thread.Condition);
    w.bestF := LAST(LONGREAL);
    w.f     := f;
    w.cntF  := 0;
    RETURN w
  END InitWF;

  (**********************************************************************)
  
TYPE
  MinMethod = { PowellLine, NewUOA, NewUOAs };

PROCEDURE DoMinimize(p      : LRVector.T;
                     rhoend  : LONGREAL;
                     func   : LRScalarField.T;
                     rhobeg : LONGREAL;
                     cutoff : LONGREAL) : LONGREAL =
  VAR
    method : MinMethod := MinMethod.NewUOAs;
  BEGIN
    CASE method OF
      MinMethod.PowellLine =>
      VAR
        dim := Matrix.Dim { NUMBER(p^), NUMBER(p^) };
        xi  := Matrix.Unit(dim);
      BEGIN
        RETURN Powell.Minimize(p, xi, rhoend (*not quite right*), func)
      END
    |
      MinMethod.NewUOAs =>
      WITH res = NewUOAs.Minimize(p, func, rhobeg, rhoend, ftarget := cutoff) DO
        Debug.Out("NewUOAs terminated : " & res.message);
        p^ := res.x^;
        RETURN res.f
      END
    |
      MinMethod.NewUOA =>
      RETURN NewUOA_M3.Minimize(p,
                                func,
                                2*NUMBER(p^)+1,
                                rhobeg,
                                rhoend (* not quite right *),
                                1000*1000)
    END
  END DoMinimize;
  
TYPE
  TestFuncType = LRScalarField.Default OBJECT
    thr : VarOptThread;
  OVERRIDES
    eval := EvalTF;
  END;
  
PROCEDURE EvalTF(t : TestFuncType; xarg : LRVector.T) : LONGREAL =
  VAR
    ss := 0.0d0;
    dbg := "TestFunc";
    x := ApplyConstraint(xarg, t.thr.radius);
  BEGIN
    (* min at 1, 1, 1, ... , 1 *)
    FOR i := FIRST(x^) TO LAST(x^) DO
      dbg := dbg & F(" %10s", LongReal(x[i], prec :=4, style := Style.Fix));
      ss := ss + Math.sqrt((1.1d0 - x[i])*(1.1d0 - x[i]))
    END;
    dbg := dbg & " = " & LongReal(ss, prec :=4, style := Style.Fix);
    Debug.Out(dbg);
    RETURN ss/100.0d0
  END EvalTF;
  
TYPE
  VarFunc = LRScalarField.Default OBJECT
    thr : VarOptThread;
    minMu  : MUTEX;
    minVal : LONGREAL;
    minAt  : LRVector.T;
  OVERRIDES
    eval := EvalVarFunc;
  END;

PROCEDURE FmtVector(x : LRVector.T) : TEXT =
  VAR
    wx := Wx.New();
  BEGIN
    Wx.PutText(wx, "[ ");
    FOR i := FIRST(x^) TO LAST(x^) DO
      Wx.PutText(wx, LongReal(x[i]));
      Wx.PutChar(wx, ' ')
    END;
    Wx.PutChar(wx, ']');
    RETURN Wx.ToText(wx)
  END FmtVector;

VAR varId : CARDINAL := 0;
    varIdMu := NEW(MUTEX);
    memoTbl := NEW(TextRefTbl.Default).init();
    memoMu := NEW(MUTEX);
    
PROCEDURE CopyV(v : LRVector.T) : LRVector.T =
  BEGIN
    WITH res = NewV(NUMBER(v^)) DO
      res^ := v^;
      RETURN res
    END
  END CopyV;

PROCEDURE EvalVarFunc(vf : VarFunc; xarg : LRVector.T) : LONGREAL =
  VAR
    id : CARDINAL;
    x := ApplyConstraint(xarg, vf.thr.radius); (* for now *)
    memoStr : TEXT;
    dummy : BOOLEAN;
    r : REFANY;
    node : VarOptNode;
  CONST
    OptResult = -1.0d10;
  BEGIN
    Debug.Out(F("Schmoozer.EvalVarFunc radius %s @ %s -> %s",
                LongReal(vf.thr.radius), FmtVector(xarg),FmtVector(x)));
    WITH an      = NUMBER(vf.thr.valPfx^),
         allVals = NEW(REF ARRAY OF Val, an+1) DO
      Debug.Out("Schmoozer.EvalVarFunc radius an=" & Int(an));
      SUBARRAY(allVals^,0,an) := vf.thr.valPfx^;
      allVals[an] := NEW(VarOptVal, s := vf.thr.varopt, argList := vf.thr.argList, x := CopyV(x));

      memoStr := FormatCommand(0, "", allVals^, FALSE, dummy);

      Debug.Out("Schmoozer.EvalVarFunc memoStr : " & memoStr);
      (* three cases here: 
         nobody did this one before, 
         someone did it and it's done,
         someone did it and it's in progress *)

      LOCK memoMu DO
        IF memoTbl.get(memoStr, r) THEN
          Debug.Out("Schmoozer.EvalVarFunc someone else running " & memoStr);
          node := r
        ELSE
          Debug.Out("Schmoozer.EvalVarFunc launching " & memoStr);
          LOCK varIdMu DO id := varId; INC(varId) END;
          WITH mu      = NEW(MUTEX),
               c       = NEW(Thread.Condition) DO
            node := NEW(VarOptNode, mu := mu, c := c);
            LOCK activeMu DO
              EVAL activeNodes.put(id, node)
            END;
            EVAL memoTbl.put(memoStr, node)
          END;
          LaunchSingleJob(allVals^, id)
        END
      END;

      Debug.Out("Schmoozer.EvalVarFunc awaiting result");
      
      LOCK node.mu DO
        WHILE NOT node.haveVal DO Thread.Wait(node.mu, node.c) END
      END;
      
      Debug.Out("Schmoozer.EvalVarFunc waking up on " & memoStr);
      IF node.validResult THEN
        (* we're looking for a BAD result so we should 
           return the neg. if maximizing something *)
        WITH res = vf.thr.varopt.optMult * node.val DO
          Debug.Out(F("EvalVarFunc returning " & LongReal(res)));
          LOCK vf.minMu DO
            IF res < vf.minVal THEN
              vf.minVal := res; vf.minAt^ := xarg^
            END
          END;
          PushMinResult(memoStr, LongReal(res));
          RETURN res
        END;
      ELSE
        Debug.Warning("Schmoozer.EvalVarFunc invalid result at " & FmtVector(xarg));
        PushMinResult(memoStr, "INVALID");
        (* the rationale for returning an "optimal" result here is that 
           we likely just broke the circuit, and the point of this optimization
           is to do precisely that! *)
        RETURN OptResult
      END
    END
  END EvalVarFunc;

VAR pushMu := NEW(MUTEX);
    
PROCEDURE PushMinResult(args, res : TEXT) =
  BEGIN
    LOCK pushMu DO
      WITH wr = FileWr.OpenAppend(resultsDn & "/optresults.dat") DO
        Wr.PutText(wr, args);
        Wr.PutChar(wr, ' ');
        Wr.PutText(wr, res);
        Wr.PutChar(wr, '\n');
        Wr.Close(wr)
      END
    END
  END PushMinResult;

PROCEDURE PushMinimum(val, radius  : LONGREAL;
                      READONLY pfx : ARRAY OF Val;
                      at           : LRVector.T) =
  VAR
    dummy : BOOLEAN;
  BEGIN
    LOCK pushMu DO
      WITH wr = FileWr.OpenAppend(resultsDn & "/optminimum.dat") DO
        Wr.PutText(wr, "opt ");
        Wr.PutText(wr, LongReal(val));
        Wr.PutText(wr, " radius ");
        Wr.PutText(wr, LongReal(radius));
        Wr.PutChar(wr, ' ');
        Wr.PutText(wr, FormatCommand(0, "", pfx, FALSE, dummy));
        FOR i := FIRST(at^) TO LAST(at^) DO
          Wr.PutChar(wr, ' ');
          Wr.PutText(wr, LongReal(at[i]))
        END;
        Wr.PutChar(wr, '\n');
        Wr.Close(wr)
      END
    END
  END PushMinimum;

PROCEDURE ApplyConstraint(x : LRVector.T; r : LONGREAL) : LRVector.T =
  VAR
    res : LRVector.T;
    normsq := 0.0d0;
    q : LONGREAL;
  BEGIN
    FOR i := FIRST(x^) TO LAST(x^) DO
      normsq := normsq + x[i]*x[i]
    END;
    IF normsq < r*r THEN RETURN x END;


    (* implement the triangle wave *)
    res := NEW(LRVector.T, NUMBER(x^));

    IF r = 0.0d0 THEN (* important special case *)
      FOR i := FIRST(x^) TO LAST(x^) DO res[i] := 0.0d0 END;
      RETURN res
    END;

    <*ASSERT normsq > 0.0d0*>
    
    WITH norm      = Math.sqrt(normsq),
         relnorm   = norm/r,
         relnormfl = FLOOR(relnorm),
         relnormx  = relnorm-FLOAT(relnormfl, LONGREAL) DO

      Debug.Out(F("ApplyConstraint norm %s relnorm %s relnormfl %s relnormx %s",
                  LongReal(norm), LongReal(relnorm), Int(relnormfl),
                  LongReal(relnormx)));
      
      IF relnormfl MOD 2 = 0 THEN
        q := relnormx
      ELSE
        q := 1.0d0 - relnormx
      END;
      (* 0 <= q <= 1 *)
      FOR i := FIRST(x^) TO LAST(x^) DO
        res[i] := r * q * x[i] / norm;
        IF ABS(res[i]) < Eps THEN res[i] := 0.0d0 END;
      END
    END;
    RETURN res
  END ApplyConstraint;
  
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
          Variety, Sweep, SweepSpecific, SpecificInt =>
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

  (* LaunchShmoo -> LaunchMesh -> LaunchList *)

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
        LOCK activeMu DO EVAL activeNodes.put(m.id, m) END
      END
    END
  END LaunchList;
  
VAR
  nextJobId := 0;
  activeMu := NEW(MUTEX);
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

PROCEDURE IsSimSetting(v : Val) : BOOLEAN =
  BEGIN
    TYPECASE v.s OF
      SingleSettings(ss) => RETURN ss.param = simP
    ELSE
      RETURN FALSE
    END
  END IsSimSetting;

PROCEDURE SettingsDbg(s : Settings) : TEXT =
  BEGIN
    TYPECASE s OF
      SingleSettings(ss) => RETURN ss.param.nm
    ELSE
      RETURN "**unknown**"
    END
  END SettingsDbg;

PROCEDURE FormatCommand(id             : CARDINAL;
                        pfx            : TEXT;
                        READONLY v     : ARRAY OF Val;
                        memoize        : BOOLEAN;
                        VAR didMemoize : BOOLEAN) : TEXT =
  VAR
    command := pfx & extraCmdHack;
    beforeSpice := command;
    afterSpice := "";
    memoId : CARDINAL;
    
  BEGIN
    didMemoize := FALSE;
    FOR t := FIRST(Tool) TO LAST(Tool) DO
      FOR i := FIRST(v) TO LAST(v) DO
        IF Verbose THEN
          Debug.Out(F("FormatCommand tool %s v %s",
                      ToolNames[t],
                      SettingsDbg(v[i].s)))
        END;
        <*ASSERT v[i] # NIL*>
        WITH fm = v[i].format(t) DO
          IF Text.Length(fm) # 0 AND NOT IsSimSetting(v[i]) THEN
            (* we skip simulator since we specify that separately *)
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

PROCEDURE IsFancyType(s : Settings) : BOOLEAN =
  BEGIN
    RETURN ISTYPE(s, Schmoo) OR ISTYPE(s, VarOpt)
  END IsFancyType;
  
PROCEDURE Add(s : Settings) =
  BEGIN
    (* invariants:
       1. at most one Schmoo/VarOpt.
       2. Schmoo/VarOpt always at the end *)
    IF IsFancyType(s) THEN
      <*ASSERT settings.size()=0 OR 
               NOT IsFancyType(settings.get(settings.size()-1)) *>
      settings.addhi(s)
    ELSE
      settings.addlo(s)
    END;

    TYPECASE s OF
      Variety(v) =>
      Debug.Out("Adding Variety " & v.param.nm)
    ELSE
      (* skip *)
    END
  END Add;
  
PROCEDURE Process(pp : ParseParams.T; READONLY a : ARRAY OF Schmooze.T)
  RAISES { ParseParams.Error } =
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

PROCEDURE Setup(pp : ParseParams.T; simPA : DiscreteParam) 
  RAISES { ParseParams.Error } =
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

PROCEDURE RunAll(pp : ParseParams.T) 
  RAISES { ParseParams.Error, OSError.E } =
  BEGIN

  pp.finish();

  FOR i := 0 TO settings.size()-1 DO
    TYPECASE settings.get(i) OF
      Variety(v) => Debug.Out("RunAll settings Variety " & v.param.nm)
    ELSE
    END
  END;
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

  user := Env.Get("USER");

  extraCmdHack := "";

PROCEDURE SetExtraCmdHack(to : TEXT) =
  BEGIN
    extraCmdHack := to
  END SetExtraCmdHack;
  
BEGIN END Schmoozer.

  


  
