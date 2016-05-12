MODULE Schmoozer EXPORTS Main;
IMPORT ProbeMode, Sim;
IMPORT RefSeq;
IMPORT Dims;
IMPORT Debug;
IMPORT RefList;
IMPORT FS;
IMPORT Time, Date;
FROM Fmt IMPORT Int, LongReal, F;
IMPORT LRPairRefTbl, LongRealPair;
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

<*FATAL Thread.Alerted*>
<*FATAL Wr.Failure*>

CONST TE = Text.Equal;

TYPE
  Param = BRANDED OBJECT 
    nm   : TEXT;
    flag : TEXT;
  METHODS
    init() : Param := InitParam;
  END;

  RealParam = Param OBJECT
    saneMin, saneMax : LONGREAL;
  END;
  
  DiscreteParam = Param OBJECT
    vals : REF TA;
  END;

  IntParam = Param OBJECT
    min, max : INTEGER;
  END;

VAR
  allParams := NEW(RefSeq.T).init();

PROCEDURE InitParam(p : Param) : Param = 
  BEGIN
    allParams.addhi(p);
    RETURN p
  END InitParam;
  
VAR
  probeP  := NEW(DiscreteParam,
                 nm := "probemode",
                 flag := "probemode",
                 vals := RT(ProbeMode.Names)).init();

  cornerP := NEW(DiscreteParam,
                 nm := "corner",
                 flag := "m",
                 vals := RT(TA { 
                                 "afnoif",
                                 "afnois",
                                 "bghlhh",
                                 "bghllh",
                                 "bghlth",
                                 "bglhll",
                                 "bglhll2",
                                 "bglhlt",
                                 "bgllll",
                                 "bgtlhh",
                                 "ffff",
                                 "ffvss",
                                 "pfff",
                                 "psss",
                                 "rafas",
                                 "rasaf",
                                 "rfafs",
                                 "rfafs_gs",
                                 "rfasf",
                                 "rfasf_gs",
                                 "rfff",
                                 "rfffaf",
                                 "rfffaf_gs",
                                 "rffs",
                                 "rffx",
                                 "rffxaf",
                                 "rfsf",
                                 "rfxf",
                                 "rfxfaf",
                                 "rsafs",
                                 "rsafs_gf",
                                 "rsasf",
                                 "rsasf_gf",
                                 "rsfs",
                                 "rssf",
                                 "rsss",
                                 "rsssas",
                                 "rsssas_gf",
                                 "rssx",
                                 "rssxas",
                                 "rsxs",
                                 "rsxsas",
                                 "ssss",
                                 "ssvff",
                                 "tttt"
  })).init();

  simP    := NEW(DiscreteParam,
                 nm := "simulator",
                 flag := "f",
                 vals := RT(Sim.Names)).init();

  vddP    := NEW(RealParam,
                 nm := "vdd",
                 flag := "vdd",
                 saneMin := 0.20d0, saneMax := 2.0d0).init();

  tempP   := NEW(RealParam,
                 nm := "temp",
                 flag := "temp",
                 saneMin := -100.0d0, saneMax := 200.0d0).init();

  spfP    := NEW(RealParam,
                 nm := "spf",
                 flag := "spf",
                 saneMin := 0.0d0, saneMax := 1.0d30).init();


  clkP    := NEW(RealParam,
                 nm := "clk",
                 flag := "clk",
                 saneMin := 100.0d6, saneMax := 20.0d9).init();

  deckP   := NEW(DiscreteParam,
                 nm := "deck",
                 flag := "deck",
                 vals := RT(TA { 
                                 "bothrs", (* s.b. default *)
                                 "writers",
                                 "readrs",
                                 "nors"
  })).init();
  
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

TYPE
  Settings = BRANDED OBJECT 
  METHODS
    n() : CARDINAL;
  END;

  SingleSettings = Settings OBJECT
    param : Param;
  END;

  Variety = SingleSettings OBJECT
    cover : REF TA;
  OVERRIDES 
    n := NVariety;
  END;

  Sweep = SingleSettings OBJECT
    min, max, step : LONGREAL;
  OVERRIDES 
    n := NSweep;
  END;

  SweepSpecific = SingleSettings OBJECT
    v : REF ARRAY OF LONGREAL;
  OVERRIDES 
    n := NSweepSpecific;
  END;

  Schmoo = Settings OBJECT
    param                      : ARRAY [0..1] OF RealParam;
    min, max, minStep, maxStep : ARRAY [0..1] OF LONGREAL;
  OVERRIDES
    n := NSchmoo;
  END;

  Job = OBJECT 
    settings : RefSeq.T; (* settings *)
  END;

  Val = OBJECT
    s : Settings;
  METHODS
    format() : TEXT := FormatV;
  END;

  StepVal = Val OBJECT i : CARDINAL END;

  SchmooVal = Val OBJECT x : LR01 END;

  SchmooInstance = OBJECT
    schmoo     : Schmoo;
    meshTab    : LRPairRefTbl.T;
    valPfx     : REF ARRAY OF Val;
    minSquares : XYList.T := NIL;
  END;

VAR schmooInstances : RefList.T := NIL;

PROCEDURE FormatV(v : Val) : TEXT =
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
      RETURN F("-%s %s", NARROW(st.s,SingleSettings).param.flag, str)
    |
      SchmooVal(sc) =>
      WITH ss = NARROW(sc.s, Schmoo) DO
        RETURN F("-%s %s -%s %s", ss.param[0].flag, LongReal(sc.x[0]), 
                                  ss.param[1].flag, LongReal(sc.x[1]))
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

TYPE
  TA = ARRAY OF TEXT;
  LA = ARRAY OF LR;
  LR = LONGREAL;
  RP01 = ARRAY [0..1] OF RealParam;
  LR01 = ARRAY [0..1] OF LR;
  I01  = ARRAY [0..1] OF INTEGER;

TYPE
  MeshNode = OBJECT
    id    : CARDINAL;
    x     : LR01;
    state := NodeState.Running; (* ?? *)
    watchers : RefList.T := NIL;
    args  : REF ARRAY OF Val;
  METHODS
    init() : MeshNode := InitMN;
  END;
  
  NodeState = {  Running ,  Fail ,  Pass ,  ErrorExit  };

CONST NodeStateNames = ARRAY NodeState OF TEXT {
                "Running", "FAIL", "PASS", "ErrorExit" };

TYPE
  Square = ARRAY [0..1] OF ARRAY [0..1] OF MeshNode;
  XYSquare = ARRAY [0..1] OF XYRow;
  XYRow = ARRAY [0..1] OF LR01;

  Watcher = OBJECT
    id     : CARDINAL;
    s      : SchmooInstance;
    square : Square;
    done   := FALSE;
  END;

PROCEDURE InitMN(mn : MeshNode) : MeshNode =
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
  RAISES { ProcUtils.ErrorExit, Rd.Failure, OSError.E, Parse } =
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

    WITH data = ProcUtils.ToText(cmd),
         rd   = TextRd.New(data) DO
      TRY
        LOOP
          WITH line = Rd.GetLine(rd) DO
            IF TextUtils.FindSub(line, user, dummy) THEN
              (* match *)
              WITH rdr = NEW(TextReader.T).init(line),
                   stateStr = rdr.nextE(" ", skipNulls := TRUE),
                   jobId    = Scan.Int(rdr.nextE(" ")) DO
                Debug.Out("SearchForLostLambs: found job " & Int(jobId));
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

  PROCEDURE ProcessOutput(mn : MeshNode; ln : TEXT) RAISES { OSError.E } =
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
        
        FOR i := FIRST(mn.args^) TO LAST(mn.args^) DO
          WITH cmd = mn.args[i].format() DO
            Wr.PutText(wr, cmd);
            Wr.PutChar(wr, '\n');
            
            Wr.PutText(r1Wr, cmd); Wr.PutChar(r1Wr, ' ');
            Wr.PutText(sWr, cmd); Wr.PutChar(sWr, ' ')
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
           mn    = NARROW(r, MeshNode) DO

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

PROCEDURE ChangedState(mn : MeshNode) =
  VAR
    p := mn.watchers;
  BEGIN
    Debug.Out(F("%s watchers", Int(RefList.Length(mn.watchers))));
    WHILE p # NIL DO
      WakeWatcher(p.head);
      p := p.tail
    END
  END ChangedState;

PROCEDURE DebugPt(nm : TEXT; x : LR01) =
  BEGIN
    Debug.Out(F("%s %12s %12s", nm, LongReal(x[0]), LongReal(x[1])))
  END DebugPt;

PROCEDURE Subdivide(w : Watcher) =
  BEGIN
    WITH x00 = w.square[0,0].x,
         x01 = w.square[0,1].x,
         x10 = w.square[1,0].x,
         x11 = w.square[1,1].x,
         
         xn =  Split(x00,x01), 
         xe =  Split(x11,x01),
         xs =  Split(x11,x10),
         xw =  Split(x00,x10),
         xc =  Split(xw ,xe ),
         
         (* x00    xn    x01
            xw     xc    xe
            x10    xs    x11 *)
         
         mm = ARRAY [0..2] OF ARRAY [0..2] OF LR01 {
                ARRAY [0..2] OF LR01 { x00, xn , x01},
                ARRAY [0..2] OF LR01 { xw , xc , xe },
                ARRAY [0..2] OF LR01 { x10, xs , x11} } DO
      
      DebugPt("x00", x00);
      DebugPt("x01", x01);
      DebugPt("x10", x10);
      DebugPt("x11", x11);
      
      DebugPt("xn ", xn);
      DebugPt("xe ", xe);
      DebugPt("xs ", xs);
      DebugPt("xw ", xw);
      DebugPt("xc ", xc);
      
      <*ASSERT x00[1] = x10[1]*>
      <*ASSERT x00[0] = x01[0]*>
      IF x11[0]-x00[0] <= w.s.schmoo.minStep[0]   AND
         x11[1]-x00[1] <= w.s.schmoo.minStep[1]     THEN
        (* achieved minstep, stop! *)
        Debug.Out("Achieved minstep, quitting here.");
        w.s.minSquares := XYList.Cons(xc, w.s.minSquares);
        RETURN
      END;
      
      FOR i := FIRST(mm) TO LAST(mm)-1 DO
        FOR j := FIRST(mm[i]) TO LAST(mm[i])-1 DO
          LaunchSquare(w.s, 
                       XYSquare { XYRow { mm[i  ,j  ], mm[i  ,j+1] },
                                  XYRow { mm[i+1,j  ], mm[i+1,j+1] }
          }
          )
        END
      END
    END
  END Subdivide;

PROCEDURE WakeWatcher(w : Watcher) =
  VAR
    count := ARRAY NodeState OF CARDINAL { 0, .. };
  BEGIN
    IF w.done THEN RETURN END; (* crud *)
    Debug.Out("WakeWatcher " & Int(w.id));
    w.done := TRUE;

    FOR i := FIRST(w.square) TO LAST(w.square) DO
      FOR j := FIRST(w.square[i]) TO LAST(w.square[i]) DO
        INC(count[w.square[i,j].state])
      END
    END;

    IF    (count[NodeState.Pass] # 0 AND count[NodeState.Fail] # 0) OR
           count[NodeState.ErrorExit] # 0 THEN
      Debug.Out("Different states!");
      Subdivide(w);
      w.done := TRUE
    ELSIF count[NodeState.Running] # 0 THEN
      Debug.Out(F("WakeWatcher: node(s) still running, going back to sleep"));
      w.done := FALSE
    ELSE
      <*ASSERT count[NodeState.Pass] = 4 OR count[NodeState.Fail] = 4*>
      Debug.Out("All states the same, nothing to do here");
      w.done := TRUE
    END
  END WakeWatcher;


PROCEDURE LaunchSchmoo(pfx : RefSeq.T; READONLY q : Dims.T; schmoo : Schmoo) =
  (* note we can launch many schmoos *)
  VAR
    valPfx := NEW(REF ARRAY OF Val, NUMBER(q));
    initSz : I01;
    initXY : REF ARRAY OF ARRAY OF LR01;
    actStep : LR01;
    inst : SchmooInstance;
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

    <*ASSERT NUMBER(q) = pfx.size()-1*>
    FOR i := 0 TO 1 DO
      initSz[i] := CEILING((schmoo.max[i]-schmoo.min[i])/schmoo.maxStep[i])+1;
      IF schmoo.max[i] = schmoo.min[i] THEN
        actStep[i] := 0.0d0
      ELSE
        actStep[i] := (schmoo.max[i]-schmoo.min[i])/FLOAT(initSz[i]-1,LONGREAL)
      END
    END;

    Debug.Out(F("LaunchShmoo initSz = %s x %s", 
                Int(initSz[0]),
                Int(initSz[1])));

    initXY   := NEW(REF ARRAY OF ARRAY OF LR01, initSz[0], initSz[1]);

    inst := NEW(SchmooInstance, 
                schmoo     := schmoo, 
                valPfx     := valPfx,
                meshTab    := NEW(LRPairRefTbl.Default).init());

    schmooInstances := RefList.Cons(inst, schmooInstances);

    FOR i := 0 TO initSz[0]-1 DO
      FOR j := 0 TO initSz[1]-1 DO
        initXY[i,j] := LR01{ schmoo.min[0]+actStep[0]*FLOAT(i,LR),
                             schmoo.min[1]+actStep[1]*FLOAT(j,LR) }
      END
    END;

    IF initSz[0] = 1 OR initSz[1] = 1 THEN
      (* user is requesting something that cant be subdivided *)
      FOR i := 0 TO initSz[0]-1 DO
        FOR j := 0 TO initSz[1]-1 DO
          LaunchIndivisible(inst, initXY[i,j])
        END
      END
    ELSE
      FOR i := 0 TO initSz[0]-2 DO
        FOR j := 0 TO initSz[1]-2 DO
          LaunchSquare(inst,
                       XYSquare { XYRow { initXY[i  ,j  ], initXY[i  ,j+1] } ,
                                  XYRow { initXY[i+1,j  ], initXY[i+1,j+1] } } )
        END
      END
    END
  END LaunchSchmoo;

PROCEDURE ToPair(x : LR01) : LongRealPair.T =
  BEGIN RETURN LongRealPair.T { x[0], x[1] } END ToPair;

PROCEDURE Split(x, y : LR01) : LR01 =
  BEGIN RETURN LR01 { (x[0]+y[0])/2.0d0, (x[1]+y[1])/2.0d0 } END Split;

VAR
  nextWatcherId := 0;

PROCEDURE LaunchIndivisible(inst : SchmooInstance; x : LR01) =
  (* called when user has requested something that cant be subdivided,
     kind of a hack! *)
  VAR
    r : REFANY;
    new : MeshNode;
  BEGIN
    IF inst.meshTab.get(ToPair(x),r) THEN
      new := r
    ELSE
      new := NEW(MeshNode, x := x).init()
    END;
    (* no watchers since we cant subdivide *)
    WITH m = new,
         an = NUMBER(inst.valPfx^),
         allVals = NEW(REF ARRAY OF Val, an) DO
      SUBARRAY(allVals^,0, NUMBER(inst.valPfx^)) := inst.valPfx^;
      allVals[an-1] := NEW(SchmooVal,
                           s := inst.schmoo,
                           x := m.x);
      LaunchSingleJob(allVals^, m.id);
      m.args := allVals;
      Debug.Out(F("activeNodes.put(%s)",Int(m.id)));
      EVAL activeNodes.put(m.id, m)
    END
  END LaunchIndivisible;

PROCEDURE LaunchSquare(inst : SchmooInstance; sq : XYSquare) =
  VAR
    new := NEW(RefSeq.T).init();
    m : Square;
    r : REFANY;
  BEGIN
    DebugPt("LaunchSquare sq[0,0]", sq[0,0]);
    DebugPt("LaunchSquare sq[0,1]", sq[0,1]);
    DebugPt("LaunchSquare sq[1,0]", sq[1,0]);
    DebugPt("LaunchSquare sq[1,1]", sq[1,1]);

    FOR i := FIRST(sq) TO LAST(sq) DO
      FOR j := FIRST(sq[i]) TO LAST(sq[i]) DO
        IF inst.meshTab.get(ToPair(sq[i,j]),r) THEN
          m[i,j] := r;
        ELSE
          m[i,j] := NEW(MeshNode, x := sq[i,j]).init();
          EVAL inst.meshTab.put(ToPair(sq[i,j]),m[i,j]);
          new.addhi(m[i,j])
        END
      END
    END;
    VAR
      w := NEW(Watcher, id := nextWatcherId, s := inst, square := m);
    BEGIN
      INC(nextWatcherId);
      FOR i := FIRST(sq) TO LAST(sq) DO
        FOR j := FIRST(sq[i]) TO LAST(sq[i]) DO
          Debug.Out(F("Adding watcher %s to cmd %s ", 
                      Int(w.id), Int(m[i,j].id)));
          m[i,j].watchers := RefList.Cons(w, m[i,j].watchers)
        END
      END;
      
      (* the watcher COULD be ready to run *)
      WakeWatcher(w)
    END;
    (* actually launch jobs *)
    FOR i := 0 TO new.size()-1 DO
      WITH m       = NARROW(new.get(i),MeshNode),
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
  END LaunchSquare;

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

PROCEDURE LaunchSingleJob(READONLY v : ARRAY OF Val; id : CARDINAL) =
  VAR
    jdn := F("%s/" & FNFmt,absRoot,Int(id));
    nbs := F("%s --log-file-dir %s", NetBatchString, rootDn);
    command := F("%s %s %s " & FNFmt & " %s ", 
                 nbs, cmdNm, absRoot, Int(id), FindSim(v));
  BEGIN
    TRY
      FS.CreateDirectory(jdn)
    EXCEPT
      OSError.E(x) =>
      Debug.Error("Couldnt create directory \""& jdn & "\" : OSError.E : " &
        AL.Format(x))
    END;
    
    FOR i := FIRST(v) TO LAST(v) DO
      <*ASSERT v[i] # NIL*>
      command := command & " " & v[i].format();
    END;
    Debug.Out(F("command %s : %s", Int(id), command));
    Wr.PutText(clogWr, F("%s %s\n", Int(id), command));
    Wr.Flush  (clogWr);
    WITH nbCmd = NEW(NbCommand.T, id := id, cmd := command) DO
      Enqueue(nbCmd);
      LOCK nbMu DO
        EVAL jobsById.put(id, nbCmd)
      END
    END
  END LaunchSingleJob;

VAR cmdQ  := NEW(NbCommandSeq.T).init();
VAR cmdMu := NEW(MUTEX);
VAR cmdC  := NEW(Thread.Condition);

PROCEDURE Enqueue(command : NbCommand.T) =
  BEGIN
    LOCK cmdMu DO cmdQ.addhi(command); Thread.Signal(cmdC) END
  END Enqueue;

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
    quota := GetLaunchQuota();
    cmd : NbCommand.T;
  BEGIN
    LOOP(*forever*)

      WHILE quota = 0 DO
        quota := GetLaunchQuota();
        IF quota = 0 THEN Thread.Pause(5.0d0) END;
      END;
      (* quota > 0 *)

      LOCK cmdMu DO
        WHILE cmdQ.size() = 0 DO Thread.Wait(cmdMu, cmdC) END;
        (* cmdQ.size() > 0 *)
        cmd := cmdQ.remlo()
      END;

      DEC(quota);

      Debug.Out(F("actually launching command, rem. quota %s : %s %s",
                  Int(quota), Int(cmd.id), cmd.cmd));

      VAR 
        success := FALSE;
        output : TEXT;
      CONST
        Attempts = 10;
      BEGIN
        FOR i := 1 TO Attempts DO
          TRY
            output := ProcUtils.ToText(cmd.cmd);
            Debug.Out("command output : " & output);

            WITH nbId = IntAfter(output, "JobID") DO
              Debug.Out("NB id is " & Int(nbId));
              LOCK nbMu DO 
                cmd.nbId := nbId;
                cmd.started := Time.Now()
              END
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
          END
        END;
        IF NOT success THEN 
          Debug.Error("Command failed repeatedly : " & cmd.cmd) 
        END
      END

    END
  END LaunchApply;

VAR launchThr := Thread.Fork(NEW(Thread.Closure, apply := LaunchApply));

PROCEDURE GetLaunchQuota() : CARDINAL =
  BEGIN 
    LOOP
      TRY
        RETURN Scan.Int(ProcUtils.ToText("nbavail")) 
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


PROCEDURE AllCornerSim() =
  BEGIN
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, 
            cover := NARROW(cornerP,DiscreteParam).vals (*RT(TA { "tttt" })*)
    ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Schmoo, 
            param   := RP01 {    vddP,     clkP },
            min     := LR01 {  0.50d0,  500.0d6 },
            max     := LR01 {  1.25d0, 2500.0d6 },
            minStep := LR01 { 0.010d0,    5.0d6 },
            maxStep := LR01 { 0.100d0,  200.0d6 }));
    Add(NEW(Sweep, param := tempP, 
            min := -40.0d0, max := 125.0d0, step := 40.0d0));
  END AllCornerSim;

PROCEDURE SimulatorSim() =
  BEGIN
    (* what's going on with temperature? *)
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa", "hspice" })));
    Add(NEW(Schmoo, 
            param   := RP01 {    vddP,     clkP },
            min     := LR01 {  0.70d0, 1000.0d6 },
            max     := LR01 {  1.05d0, 1500.0d6 },
            minStep := LR01 { 0.050d0,   10.0d6 },
            maxStep := LR01 { 0.100d0,   50.0d6 }));
    Add(NEW(Sweep, param := tempP, 
            min := -40.0d0, max := 125.0d0, step := 40.0d0));
  END SimulatorSim;

PROCEDURE XATempSim() =
  BEGIN
    (* what's going on with temperature? *)
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa", "hspice" })));
    Add(NEW(Schmoo, 
            param   := RP01 {    vddP,     clkP },
            min     := LR01 {  0.65d0, 1000.0d6 },
            max     := LR01 {  1.00d0, 2200.0d6 },
            minStep := LR01 { 0.005d0,   15.0d6 },
            maxStep := LR01 { 0.050d0,  150.0d6 }));
    Add(NEW(Sweep, param := tempP, 
            min := -40.0d0, max := 125.0d0, step := 32.5d0));
  END XATempSim;

PROCEDURE WideSim() =
  BEGIN
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Schmoo,
            param   := RP01 {    vddP,     clkP },
            min     := LR01 {  0.00d0,  200.0d6 },
            max     := LR01 {  1.50d0, 3200.0d6 },
            minStep := LR01 { 0.0075d0,  10.0d6 },
            maxStep := LR01 { 0.100d0,  200.0d6 }));
    Add(NEW(Sweep, param := tempP,
            min := 25.0d0, max := 25.0d0, step := 1.0d0));
  END WideSim;

PROCEDURE SpfSim() =
  BEGIN
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Schmoo, 
            param   := RP01 {    vddP,     clkP },
            min     := LR01 {  0.50d0,  800.0d6 },
            max     := LR01 {  1.00d0, 2200.0d6 },
            minStep := LR01 { 0.006d0,   20.0d6 },
            maxStep := LR01 { 0.048d0,  160.0d6 }));
    Add(NEW(Sweep, param := tempP, 
            min := 25.0d0, max := 25.0d0, step := 1.0d0));
    Add(NEW(SweepSpecific, param := spfP,
            v := RL(LA { 0.10d0, 0.03d0, 0.01d0, 0.003d0, 0.001d0, 0.0003d0, 0.0d0 })));
  END SpfSim;

PROCEDURE QuickSim() =
  BEGIN
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Schmoo, 
            param   := RP01 {    vddP,     clkP },
            min     := LR01 {  0.50d0,  600.0d6 },
            max     := LR01 {  1.00d0, 2600.0d6 },
            minStep := LR01 {  0.001d0,   4.0d6 },
            maxStep := LR01 {  0.050d0,  200.0d6 }));
    Add(NEW(Sweep, param := tempP, 
            min := 25.0d0, max := 25.0d0, step := 1.0d0));
  END QuickSim;

PROCEDURE LatchSim() =
  BEGIN
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Sweep, param := tempP, 
            min := 25.0d0, max := 25.0d0, step := 1.0d0));

    (* for now: bigtime hack, add this last: *)
    Add(NEW(Variety, param := deckP  , cover := RT(TA {   "bothrs", "readrs", "writers", "nors" })));
    Add(NEW(Schmoo, 
            param   := RP01 {    vddP,     clkP },
            min     := LR01 {  0.50d0,  300.0d6 },
            max     := LR01 {  1.20d0, 2600.0d6 },
            minStep := LR01 {  0.002d0,   4.0d6 },
            maxStep := LR01 {  0.050d0,  200.0d6 }));
  END LatchSim;

PROCEDURE ThreeCornerSim() =
  BEGIN
    (* what's going on with temperature? *)
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "ssss", "tttt", "ffff" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Schmoo, 
            param   := RP01 {    vddP,     clkP },
            min     := LR01 {  0.50d0,  800.0d6 },
            max     := LR01 {  1.30d0, 2600.0d6 },
            minStep := LR01 { 0.010d0,   30.0d6 },
            maxStep := LR01 { 0.100d0,  300.0d6 }));
    Add(NEW(Sweep, param := tempP, 
            min := -40.0d0, max := 125.0d0, step := 32.5d0));
  END ThreeCornerSim;

PROCEDURE SevenCornerSim() =
  BEGIN
    (* what's going on with temperature? *)
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := 
          RT(TA { "rfff","tttt","rffs","rfsf","rsss","rssf","rsfs"  }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Schmoo, 
            param   := RP01 {    vddP,     clkP },
            min     := LR01 {  0.50d0,  800.0d6 },
            max     := LR01 {  1.30d0, 2600.0d6 },
            minStep := LR01 { 0.020d0,   45.0d6 },
            maxStep := LR01 { 0.100d0,  225.0d6 }));
    Add(NEW(Sweep, param := tempP, 
            min := -40.0d0, max := 125.0d0, step := 65.0d0));
  END SevenCornerSim;

PROCEDURE TestSim() =
  BEGIN
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Schmoo, 
            param   := RP01 {    vddP,     clkP },
            min     := LR01 {  0.75d0, 1000.0d6 },
            max     := LR01 {  0.85d0, 1010.0d6 },
            minStep := LR01 { 0.001d0,    1.0d6 },
            maxStep := LR01 { 0.100d0,   10.0d6 }));
    Add(NEW(Sweep, param := tempP, 
            min := 0.0d0, max := 0.0d0, step := 20.0d0));
  END TestSim;

PROCEDURE SingleSim() =
  BEGIN
    Add(NEW(Variety, param := probeP , cover := RT(TA {   "io" })));
    Add(NEW(Variety, param := cornerP, cover := RT(TA { "tttt" }) ));
    Add(NEW(Variety, param := simP   , cover := RT(TA {   "xa" })));
    Add(NEW(Schmoo, 
            param   := RP01 {    vddP,     clkP },
            min     := LR01 {  0.85d0, 1000.0d6 },
            max     := LR01 {  0.85d0, 1000.0d6 },
            minStep := LR01 { 0.100d0,   10.0d6 },
            maxStep := LR01 { 0.100d0,   10.0d6 }));
    Add(NEW(Sweep, param := tempP, 
            min := 0.0d0, max := 0.0d0, step := 20.0d0));
  END SingleSim;



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
CONST
  TestOnly   = FALSE;
  TempRange  = TRUE;
  AllCorners = TRUE;
  BothSims   = FALSE;
BEGIN
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

(*  SimulatorSim();*)

(*  SevenCornerSim();*)
(*  WideSim(); *)
  LatchSim();
(*  SingleSim(); *)
(*  SpfSim(); *)

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
        FOR i := FIRST(si.valPfx^) TO LAST(si.valPfx^) DO
          WITH cmd = si.valPfx[i].format() DO fn := fn & cmd END
        END;

        fn := TextUtils.Replace(fn, " ", "__");
        fn := TextUtils.Replace(fn, "-", "_");

        wr := FileWr.Open(rootDn & "/" & fn & "_boundary.dat");
        VAR q := si.minSquares; BEGIN
          WHILE q # NIL DO
            Wr.PutText(wr, F("%s %s\n", 
                             LongReal(q.head[0]), 
                             LongReal(q.head[1])));
            q := q.tail
          END
        END;
        Wr.Close(wr)
      END;
      p := p.tail
    END
  END;

  Wr.Close(clogWr)
END Schmoozer.

  


  
