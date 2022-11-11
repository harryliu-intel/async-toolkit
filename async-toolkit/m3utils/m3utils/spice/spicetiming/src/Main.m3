MODULE Main;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT Debug;
IMPORT Params;
IMPORT Pathname;
IMPORT SpiceFormat;
IMPORT SpiceError;
IMPORT AL;
IMPORT Rd, FileRd;
IMPORT OSError;
FROM Fmt IMPORT F, Int, LongReal;
IMPORT Trace;
IMPORT RegEx;
IMPORT SpiceCircuit;
IMPORT SpiceObject;
IMPORT Text;
IMPORT TraceRep;
IMPORT CitTextUtils;
IMPORT TextCardTbl;
IMPORT TextSeq;
IMPORT TransitionFinder; FROM TransitionFinder IMPORT Index;
IMPORT Transition;
IMPORT TransitionSeq;
IMPORT FileWr;
IMPORT CheckDir;
IMPORT CheckMode;
IMPORT MarginMeasurement;
IMPORT MarginScenario;
IMPORT MarginMeasurementSeq;
IMPORT MarginDump;

CONST
  Usage = "";
  TE = Text.Equal;
  LR = LongReal;
  QuickMargins = 1000;

CONST
  Up = CheckDir.T { 1 };
  Dn = CheckDir.T { -1 };
  UD = CheckDir.T { -1, 1 };

TYPE
  Arc = RECORD
    fr, to : TEXT;
    mode     : CheckMode.T;
    clkDir   : Transition.Dir;
    dataDir  : CheckDir.T;
  END;

  ArcArr = ARRAY [ 0 .. MaxArcs - 1 ] OF Arc;
  
  LatchSpec = RECORD
    typeNamePattern : TEXT;
    arcs            : ArcArr;
  END;

CONST
  MaxArcs = 4;
  NoArc   = Arc { NIL, NIL, FIRST(CheckMode.T), 0, Dn };

  VendorLatch = LatchSpec {
  "vendor_D_intel_D_g1i_D_l",
  ArcArr {
    Arc { "d", "clk", CheckMode.T.Setu, 1, UD },
    Arc { "d", "clk", CheckMode.T.Hold, 1, UD },
    NoArc,
    NoArc
  }};

  TinyLatch = LatchSpec {
  "D_TINY_U_LATCH_D_a.v.",
  ArcArr {
    Arc { "D",    "CLK", CheckMode.T.Setu,  1, Up },
    Arc { "D", "_U_CLK", CheckMode.T.Setu, -1, Dn },
    Arc { "D",    "CLK", CheckMode.T.Hold,  1, Up },
    Arc { "D", "_U_CLK", CheckMode.T.Hold, -1, Dn }
  }};
  
  LatchSpecs = ARRAY OF LatchSpec { VendorLatch, TinyLatch };

(*
 
Per Andrew 11/9/2022:

The regular/vendor latch:

setup margin is:
  min time of any change in D to rising CLK

hold margin is:
  min time of falling CLK to any change in D

Your TINY_LATCH:

setup margin is:
  min of
    min time of rising D to rising CLK
    min time of falling D to falling _CLK

hold margin is:
  min of
    min time of falling CLK to rising D
    min time of rising _CLK to falling D

... see below for more detailed timing diagrams ...

*)

  
PROCEDURE DoOneLatchSpec(db             : MarginMeasurementSeq.T;
                         sf             : SpiceFormat.T;
                         rootCkt        : SpiceCircuit.T;
                         READONLY spec  : LatchSpec) =
  (* this is for a flattened design with one level of subcircuit remaining *)
  CONST
    Verbose = FALSE;
  VAR
    regEx := RegEx.Compile(spec.typeNamePattern);
    elems := rootCkt.elements;
    subCkt : SpiceCircuit.T;
  BEGIN
    IF Verbose THEN
      Debug.Out(F("DoOneLatchSpec, elems.size=%s, subckts=%s, last=%s",
                  Int(elems.size()),
                  Int(spice.subCkts.size()),
                  spice.subCktNames.get(spice.subCktNames.size() - 1)
      ))
    END;
    
    FOR i := 0 TO elems.size() - 1 DO
      IF quick AND nMargins >= QuickMargins THEN RETURN END;
      
      WITH elem = elems.get(i) DO
        TYPECASE elem OF
          SpiceObject.X (x) =>

          IF Verbose THEN
            Debug.Out(F("X object nm %s type %s", x.name, x.type))
          END;
          
          IF RegEx.Execute(regEx, x.type) # -1 THEN
            (* it's a match *)
            Debug.Out(F("Latch search %s matches type %s , instance %s",
                        spec.typeNamePattern,
                        x.type,
                        x.name));

            (* look up type record *)
            WITH foundType = sf.subCkts.get(x.type, subCkt) DO
              IF NOT foundType THEN
                Debug.Error(F("Can't find SUBCKT %s", x.type))
              END;
              CheckLatch(db, x, subCkt, spec.arcs, trace) 
            END
          END
        ELSE
            (* skip *)
        END
      END
    END
  END DoOneLatchSpec;

PROCEDURE FindFormalIdx(s : SpiceCircuit.T; formal : TEXT) : CARDINAL =
  BEGIN
    <*ASSERT formal # NIL *>
    FOR i := 0 TO s.params.size() - 1 DO
      WITH pname = s.params.get(i) DO
        IF TE(pname, formal) THEN
          RETURN i
        END
      END
    END;
    Debug.Error(F("Can't find terminal named %s in SUBCKT %s",
                  formal, s.name));
    <*ASSERT FALSE*>
  END FindFormalIdx;

PROCEDURE CheckLatch(db              : MarginMeasurementSeq.T;
                     x               : SpiceObject.X; (* instantiation *)
                     subCkt          : SpiceCircuit.T;(* of what *)
                     READONLY arcs   : ARRAY OF Arc;  (* ...validate against *)
                     trace           : Trace.T
  ) =
  VAR
    frTrIdx, toTrIdx     : CARDINAL;
    
    posClkIdx, posDatIdx : CARDINAL;
    posClkNm , posDatNm  : TEXT;
    gotPos := FALSE;

    negClkIdx            : CARDINAL;
    negClkNm             : TEXT;
    gotNeg := FALSE;
  BEGIN
    Debug.Out(F("Checking latch %s of type %s", x.name, subCkt.name));
    
    FOR i := FIRST(arcs) TO LAST(arcs) DO
      IF arcs[i].fr # NIL THEN
        WITH arc     = arcs[i],
             frIdx = FindFormalIdx(subCkt, arc.fr),
             toIdx   = FindFormalIdx(subCkt, arc.to),
             frNm  = x.terminals.get(frIdx),
             toNm    = x.terminals.get(toIdx),
             hadFrTr = trace.getNodeIdx(frNm, frTrIdx),
             hadToTr   = trace.getNodeIdx(toNm, toTrIdx) DO
          Debug.Out(F("verifying path %s (%s) -> %s (%s)",
                      frNm, Int(frIdx),
                      toNm, Int(toIdx)));
          IF NOT hadFrTr AND NOT hadToTr THEN
            Debug.Warning(F("Can't find fr,to nodes %s,%s in trace",
                            frNm, toNm))
          ELSIF NOT hadFrTr THEN
            Debug.Warning(F("Can't find fr node %s in trace",
                            frNm))
          ELSIF NOT hadToTr THEN
            Debug.Warning(F("Can't find to node %s in trace", toNm))
          ELSE
            CASE arc.mode OF
              CheckMode.T.Setu =>
              EVAL CheckMargin(db,
                               arc.dataDir, arc.clkDir,
                               frTrIdx, frNm, toTrIdx, toNm,
                               MeasureSetup, "setup")
            |
              CheckMode.T.Hold =>
              EVAL CheckMargin(db,
                               arc.dataDir, arc.clkDir,
                               frTrIdx, frNm, toTrIdx, toNm,
                               MeasureHold, "hold")
            END;

            (* remember positive clock, if any *)
            IF arc.clkDir IN Up THEN
              gotPos    := TRUE;
              posClkIdx := toTrIdx;
              posClkNm  := toNm;
              posDatIdx := frTrIdx;
              posDatNm  := frNm;
            END;

            (* remember negative clock, if any *)
            IF arc.clkDir IN Dn THEN
              gotNeg    := TRUE;
              negClkIdx := toTrIdx;
              negClkNm  := toNm;
            END

          END
        END
      END
    END;

    (* check for glitches *)
    IF gotPos THEN
      EVAL CheckMargin(db,
                       UD, +1,
                       posDatIdx, posDatNm, posClkIdx, posClkNm,
                       MeasureGlitchwidth, "glitchwidth")
    ELSE
      Debug.Warning(F("No pos clk/data for latch %s", x.name))
    END;

    (* check for clock pulse widths *)
    IF gotPos THEN
      EVAL CheckMargin(db,
                       UD, +1,
                       posDatIdx, posDatNm, (* ignored *)
                       posClkIdx, posClkNm,
                       MeasurePulsewidth, "pulsewidth-pos")
    END;
   
    IF gotNeg THEN
      EVAL CheckMargin(db,
                       UD, -1,
                       posDatIdx, posDatNm, (* ignored *)
                       negClkIdx, negClkNm,
                       MeasurePulsewidth, "pulsewidth-neg")
    END;
   
    

  END CheckLatch;


(* 
Pulse latch timing:

             ------                  ------
            |      |                |      |
phi         |      |                |      |
            |      |                |      |
   ---------        ----------------        ------
                   .                .      .
                   .                .      .
                   .                .      .
                   .                .      .
                   .     -------------------------------
                   .    |           .      . 
dat                .    |           .      .
                   .    |           .      .
   ----------------.----            .      .
                   <----><----------><----->
                     t_h      t_s      t_p

and we have t_cyc = t_h + t_s + t_p

Now if the edge is within the clock pulse:


             ------                  ------
            |      |                |      |
phi         |      |                |      |
            |      |                |      |
   ---------        ----------------        ------
                   .                .      .
                   .                .      .
                   .                .      .
                   .                .      .
                   .                .   ----------------
                   .                .  |   . 
dat                .                .  |   .
                   .                .  |   .
   ----------------.----------------.--    .
                                    <-><--->
                                   -t_s -t_h
                                    <------>
                                      t_p

Now we have t_s and t_h negative, and t_p + t_s + t_h = 0.

One measurement left, the "glitch width":

             ------                  ------
            |      |                |      |
phi         |      |                |      |
            |      |                |      |
   ---------        ----------------        ------
                   .                .      .
                   .                .      .
                   .                .      .
                   .                .      .
                   .     XXXXXX-------------------------
                   .    |XXXXXX|    .      . 
dat                .    |XXXXXX|    .      .
                   .    |XXXXXX|    .      .
   ----------------.---- XXXXXX     .      .
                   <----><-----><---><----->
                     t_h   t_g   t_s   t_p

and we have t_cyc = t_h + t_s + t_p + t_g

*)

CONST
  Fail = LAST(LONGREAL);

PROCEDURE MeasureSetup(clkIdx   : CARDINAL;
                       data     : TransitionSeq.T;
                       clk      : TransitionSeq.T;
                       dataDir  : CheckDir.T) : LONGREAL =

  (* here we are measuring the pulse timing setup for a signal relative to
     a leading (enabling) clock edge *)
  VAR
    clkTrans := clk.get(clkIdx);
    disIdx   : CARDINAL;
    disTrans : Transition.T;
    dataIdx : Index;
    dataTrans : Transition.T;
  BEGIN
    (* find disabling edge of clock, if none, we can't measure *)
    (* distrans is what defines the END OF THE CURRENT CYCLE *)
    disIdx := clkIdx + 1;
    IF disIdx = clk.size() THEN RETURN Fail END;

    (* find disabling transition *)
    disTrans := clk.get(disIdx);
    
    (* find previous transition of data *)
    dataIdx := TransitionFinder.FindFloorIdx(data, disTrans.at);

    (* now look for the direction of data transition we are sensitive to *)
    WHILE dataIdx # -1 DO
      dataTrans := data.get(dataIdx);
      IF dataTrans.dir IN dataDir THEN EXIT END;
      DEC(dataIdx)
    END;

    IF dataIdx = -1 THEN
      RETURN Fail
    END;

    (* find previous transition of clock and check whether data transition
       was after *)
    
    IF clkIdx # 0 THEN
      WITH prevDis = clk.get(clkIdx - 1) DO
        IF dataTrans.at <= prevDis.at THEN
          (* there were no transitions on this cycle *)
          RETURN Fail
        END
      END
    END;

    (* we had a transition that we are sensitive to DURING the cycle *)
    WITH setupTime = clkTrans.at - dataTrans.at DO
      Debug.Out(F("Measured setup from edge at %s, setup %s",
                  LR(clkTrans.at), LR(setupTime)));
      RETURN setupTime
    END
  END MeasureSetup;

PROCEDURE MeasureHold(clkIdx   : CARDINAL;
                      data     : TransitionSeq.T;
                      clk      : TransitionSeq.T;
                      dataDir  : CheckDir.T) : LONGREAL =

  (* here we are measuring the pulse timing setup for a signal relative to
     a leading (enabling) clock edge *)
  VAR
    clkTrans := clk.get(clkIdx);
    disIdx   : CARDINAL;
    disTrans : Transition.T;
    dataIdx : Index;
    dataTrans : Transition.T;
    nTrans := data.size();
  BEGIN
    (* find disabling edge of clock, if none, we can't measure *)
    disIdx := clkIdx + 1;
    IF disIdx = clk.size() THEN RETURN Fail END;

    (* find disabling transition *)
    disTrans := clk.get(disIdx);

    (* find next transition after enabling edge *)
    (* first find previous transition of data before enabling edge *)
    dataIdx := TransitionFinder.FindFloorIdx(data, clkTrans.at);

    INC(dataIdx);

    (* now we point to the next transition *)

    
    (* now look for the direction of data transition we are sensitive to *)
    WHILE dataIdx # nTrans DO
      dataTrans := data.get(dataIdx);
      IF dataTrans.dir IN dataDir THEN EXIT END;
      INC(dataIdx)
    END;

    IF dataIdx = nTrans THEN
      RETURN Fail
    END;

    (* find next transition of clock and check whether data transition
       was after *)
    
    IF disIdx # clk.size() - 1 THEN
      WITH nextEna = clk.get(disIdx + 1) DO
        IF dataTrans.at > nextEna.at THEN
          (* there were no transitions on this cycle *)
          RETURN Fail
        END
      END
    END;

    (* we had a transition that we are sensitive to DURING the cycle *)
    WITH holdTime = dataTrans.at - disTrans.at DO
      Debug.Out(F("Measured hold from edge at %s, hold %s",
                  LR(clkTrans.at), LR(holdTime)));
      RETURN holdTime
    END
  END MeasureHold;

PROCEDURE MeasurePulsewidth(clkIdx   : CARDINAL;
                            <*UNUSED*>data     : TransitionSeq.T;
                            clk      : TransitionSeq.T;
                            <*UNUSED*>dataDir  : CheckDir.T) : LONGREAL =

  (* measure pulse width of clock pulse *)
  VAR
    clkTrans := clk.get(clkIdx);
    disIdx   : CARDINAL;
    disTrans : Transition.T;
  BEGIN
    (* find disabling edge of clock, if none, we can't measure *)
    disIdx := clkIdx + 1;
    IF disIdx = clk.size() THEN RETURN Fail END;

    (* find disabling transition *)
    disTrans := clk.get(disIdx);


    (* we had a transition that we are sensitive to DURING the cycle *)
    WITH pulseTime = disTrans.at - clkTrans.at DO
      Debug.Out(F("Measured pulse from edge at %s, pulse %s",
                  LR(clkTrans.at), LR(pulseTime)));
      RETURN pulseTime
    END
  END MeasurePulsewidth;

PROCEDURE MeasureGlitchwidth(clkIdx   : CARDINAL;
                             data     : TransitionSeq.T;
                             clk      : TransitionSeq.T;
                             <*UNUSED*>dataDir  : CheckDir.T) : LONGREAL =

  (* here we are measuring the 1/length of the glitchy period before an active edge of
the clock *)

  VAR
    clkTrans := clk.get(clkIdx);
    disIdx       : CARDINAL;
    disTrans     : Transition.T;

    prvIdx       : Index;
    prvTrans     : Transition.T;
    
    data0Idx     : Index;
    data0Trans   : Transition.T;

    data1Idx     : Index;
    data1Trans   : Transition.T;

  BEGIN
    (* find disabling edge of clock, if none, we can't measure *)
    (* distrans is what defines the END OF THE CURRENT CYCLE *)
    disIdx := clkIdx + 1;
    IF disIdx = clk.size() THEN RETURN Fail END;

    (* find disabling transition *)
    disTrans := clk.get(disIdx);

    (* find previous clock edge *)
    prvIdx := clkIdx - 1;
    IF prvIdx = -1 THEN RETURN Fail END;

    prvTrans := clk.get(prvIdx);
    
    (* find previous transition of data -- this is the LAST transition we care about *)
    data1Idx := TransitionFinder.FindFloorIdx(data, disTrans.at);

    data0Idx := TransitionFinder.FindFloorIdx(data, prvTrans.at) + 1;

    IF data1Idx = -1 OR data0Idx = 0 THEN
      RETURN Fail
    END;

    IF data0Trans.at < prvTrans.at OR data1Trans.at < prvTrans.at THEN
      RETURN Fail
    END;
    
    VAR
      glitchWidth := data1Trans.at - data0Trans.at;
      ret : LONGREAL;
    BEGIN
      IF glitchWidth = 0.0d0 THEN
        ret := LAST(LONGREAL)
      ELSE
        ret := 1.0d0/ret
      END;
      
      Debug.Out(F("Measured glitchwidth from edge at %s, glitchwidth %s ret %s",
                  LR(clkTrans.at), LR(glitchWidth), LR(ret)));
      RETURN ret
    END
  END MeasureGlitchwidth;

TYPE Checker = PROCEDURE(clkIdx   : CARDINAL;
                         data     : TransitionSeq.T;
                         clk      : TransitionSeq.T;
                         dataDir  : CheckDir.T) : LONGREAL;

VAR nMargins := 0;
  
PROCEDURE CheckMargin(db                : MarginMeasurementSeq.T;
                      datDir            : CheckDir.T;
                      clkDir            : Transition.Dir;
                      datTrIdx          : CARDINAL;
                      datNm             : TEXT;
                      clkTrIdx          : CARDINAL;
                      clkNm             : TEXT;
                      checker           : Checker;
                      tag               : TEXT ) : LONGREAL =
  VAR
    minMargin := LAST(LONGREAL);
    minMarginAt := FIRST(LONGREAL);
  BEGIN
    INC(nMargins);
    Debug.Out(F("CheckMargin : clkDir %s datdir %s clkNm %s datNm %s tag %s",
                Int(clkDir),
                CheckDir.Fmt(datDir),
                clkNm,
                datNm,
                tag));
    
    WITH datTrans = TransitionFinder.FilterTime(
                        tranFinder.forNode(datTrIdx),
                        resetTime, LAST(LONGREAL)),
         clkTrans = TransitionFinder.FilterTime(
                            tranFinder.forNode(clkTrIdx),
                        resetTime, LAST(LONGREAL)) DO
      
      (* hmm we really should be looking for glitches here, too! *)
      
      Debug.Out(F("data %s : %s transitions",
                  datNm, Int(datTrans.size())));
      Debug.Out(F("clk  %s : %s active transitions",
                  clkNm, Int(clkTrans.size())));

      FOR i := 0 TO clkTrans.size() - 1 DO
        WITH ct = clkTrans.get(i) DO
          <*ASSERT ct.dir IN UD*>
          IF ct.dir = clkDir THEN
            WITH thisMargin = checker(i,
                                      datTrans,
                                      clkTrans,
                                      datDir) DO
              IF thisMargin < minMargin THEN
                minMargin := thisMargin;
                minMarginAt := ct.at
              END
            END
          END
        END
      END
    END;

    WITH new = MarginMeasurement.T {
      MarginScenario.T { datDir, clkDir, datNm, clkNm, tag },
      minMargin, minMarginAt } DO
      db.addhi(new)
    END;
    
    RETURN minMargin
  END CheckMargin;
        
CONST
  DoMapTraceNames = TRUE;

TYPE Mapper = PROCEDURE(t : TEXT) : TEXT;
     
PROCEDURE MapTraceNames(trace : Trace.T; mapper : Mapper) =
  VAR
    fIter := trace.fwdTbl.iterate();
    newF  := NEW(TextCardTbl.Default).init();
    
    rIter := trace.revTbl.iterate();
    t : TEXT;
    c : CARDINAL;
    s : TextSeq.T;
  BEGIN
    WHILE fIter.next(t, c) DO
      EVAL newF.put(mapper(t), c)
    END;
    trace.fwdTbl := newF;

    WHILE rIter.next(c, s) DO
      FOR i := 0 TO s.size() - 1 DO
        WITH o = s.get(i),
             n = mapper(o) DO
          s.put(i, n)
        END
      END
    END
  END MapTraceNames;

PROCEDURE RemoveX(nm : TEXT) : TEXT =
  BEGIN
    RETURN CitTextUtils.Replace(nm, "X", "")
  END RemoveX;

VAR
  pp                          := NEW(ParseParams.T).init(Stdio.stderr);
  spiceFn    : Pathname.T     := "xa.sp";
  spice      : SpiceFormat.T;
  traceRt    : Pathname.T     := "xa";
  trace      : Trace.T;
  rootType   : TEXT := NIL;
  rootCkt    : SpiceCircuit.T;
  tranFinder : TransitionFinder.T;
  vdd                         := 0.75d0;
  resetTime                   := 10.0d-9;
  warnWr                      := FileWr.Open("spicetiming.warn");
  quick      : BOOLEAN;
  
BEGIN
  Debug.AddWarnStream(warnWr);
  
  TRY
    quick := pp.keywordPresent("-quick");
    
    IF pp.keywordPresent("-i") THEN
      spiceFn := pp.getNext()
    END;
    IF pp.keywordPresent("-t") THEN
      traceRt := pp.getNext()
    END;
    IF pp.keywordPresent("-root") THEN
      rootType := pp.getNext()
    END;
    IF pp.keywordPresent("-vdd") THEN
      vdd := pp.getNextLongReal()
    END;

    pp.skipParsed();
    pp.finish()
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command-line parameters\nUsage: " & Params.Get(0) & " " & Usage)
  END;

  TRY
    trace := NEW(Trace.T).init(traceRt)
  EXCEPT
    OSError.E(x) => Debug.Error("Trouble opening input trace : OSError.E : " & AL.Format(x))
  |
    Rd.Failure(x) => Debug.Error("Trouble opening input trace : Rd.Failure : " & AL.Format(x))
  |
    Rd.EndOfFile =>
    Debug.Error(F("Short read opening input trace"))
  END;

  tranFinder := NEW(TransitionFinder.T).init(trace, vdd / 2.0d0, vdd / 10.0d0);
  
  (* map names *)

  IF DoMapTraceNames THEN
    MapTraceNames(trace, RemoveX)
  END;
  
  TRY
    WITH spiceRd = FileRd.Open(spiceFn) DO
      spice := SpiceFormat.ParseSpice(spiceRd, ".", spiceFn);
      Rd.Close(spiceRd)
    END
  EXCEPT
    OSError.E(e) =>
    Debug.Error(F("Can't open top level file %s : OSError.E : %s",
                  spiceFn, AL.Format(e)))
  |
    SpiceError.E(e) =>
    Debug.Error(F("Parsing input : caught SpiceError.E : %s at line %s of file %s",
                  e.msg, Int(e.lNo), Debug.UnNil(e.fn)))
  END;

  IF rootType = NIL THEN
    rootCkt := spice.topCkt
  ELSE
    WITH hadIt = spice.subCkts.get(rootType, rootCkt) DO
      IF NOT hadIt THEN
        Debug.Error(F("Unknown root type %s", rootType))
      END
    END
  END;

  WITH db = NEW(MarginMeasurementSeq.T).init() DO
    FOR i := FIRST(LatchSpecs) TO LAST(LatchSpecs) DO
      IF quick AND nMargins >= QuickMargins THEN EXIT END;
      
      DoOneLatchSpec(db, spice, rootCkt, LatchSpecs[i])
    END;

    MarginDump.Do(db)
  END;

END Main.
