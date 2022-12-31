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
IMPORT TextTextTbl;
IMPORT Wx;
IMPORT ProcUtils;
IMPORT TextWr;
IMPORT Wr;
IMPORT TextIntTbl;
IMPORT CardSeq;
IMPORT Thread;

<*FATAL Thread.Alerted*>

CONST
  Usage = "";
  TE = Text.Equal;
  LR = LongReal;
  QuickMargins = 1000;

VAR
  Verbose := Debug.DebugThis("spicetiming");
  
CONST
  Up = CheckDir.T { 1 };
  Dn = CheckDir.T { -1 };
  UD = CheckDir.T { -1, 1 };

TYPE
  Node = RECORD
    nm       : TEXT;
    internal : BOOLEAN;
  END;

  N = Node;
  
  Arc = RECORD
    fr, to   : Node;
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
  MaxArcs = 6;
  NoNode = N { NIL, FALSE };
  NoArc   = Arc { NoNode, NoNode, FIRST(CheckMode.T), 0, Dn };

  VendorLatch = LatchSpec {
  "vendor_D_intel_D_g1i_D_l",
  ArcArr {
    Arc { N { "d", FALSE },   N { "clk", FALSE }, CheckMode.T.Setu,  1, UD },
    Arc { N { "d", FALSE },   N { "clk", FALSE }, CheckMode.T.Hold,  1, UD },
    Arc { N { "d", FALSE },   N { "clk", FALSE }, CheckMode.T.Hold,  1, UD },

    (* pulse width arcs are relative to the nc1 inverted internal clock *)
    Arc { N { "nk2", TRUE }, N { "clk", FALSE }, CheckMode.T.Puls,  1, Up },
    Arc { N { "nk2", TRUE }, N { "nc1", TRUE  }, CheckMode.T.Puls, -1, Dn },
    NoArc,
    ..
  }};

  TinyLatch = LatchSpec {
  "D_TINY_U_LATCH_D_a.v.",
  ArcArr {
    Arc { N { "D",  FALSE }, N { "CLK"   , FALSE }, CheckMode.T.Setu,  1, Up },
    Arc { N { "D",  FALSE }, N { "_U_CLK", FALSE }, CheckMode.T.Setu, -1, Dn },
    Arc { N { "D",  FALSE }, N { "CLK"   , FALSE }, CheckMode.T.Hold,  1, Up },
    Arc { N { "D",  FALSE }, N { "_U_CLK", FALSE }, CheckMode.T.Hold, -1, Dn },
    Arc { N { "nk", TRUE  }, N { "CLK"   , FALSE }, CheckMode.T.Puls,  1, Up },
    Arc { N { "nk", TRUE  }, N { "_U_CLK", FALSE }, CheckMode.T.Puls, -1, Dn }
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

PROCEDURE FindHierName(instance : SpiceObject.T;
                       subCkt   : SpiceCircuit.T;
                       node     : Node;
                       mapper   : Mapper) : TEXT =
  BEGIN
    IF node.internal THEN
      WITH tent = instance.name & "_D_" & node.nm DO
        IF mapper = NIL THEN
          RETURN tent
        ELSE
          RETURN mapper(tent)
        END
      END
    ELSE
      WITH idx = FindFormalIdx(subCkt, node.nm),
           nm  = instance.terminals.get(idx) DO
        RETURN (nm) (* should this be put through mapper ? *)
      END
    END
  END FindHierName;

PROCEDURE DoOneLatchSpec(db             : MarginMeasurementSeq.T;
                         sf             : SpiceFormat.T;
                         rootCkt        : SpiceCircuit.T;
                         READONLY spec  : LatchSpec;
                         mapper         : Mapper) =
  (* this is for a flattened design with one level of subcircuit remaining *)
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
              CheckLatch(db, x, subCkt, spec.arcs, trace, mapper) 
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
                     trace           : Trace.T;
                     mapper          : Mapper
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
      IF arcs[i].fr # NoNode THEN
        WITH arc     = arcs[i],
             frNm    = FindHierName(x, subCkt, arc.fr, mapper),
             toNm    = FindHierName(x, subCkt, arc.to, mapper),
             hadFrTr = trace.getNodeIdx(frNm, frTrIdx),
             hadToTr = trace.getNodeIdx(toNm, toTrIdx) DO
          Debug.Out(F("verifying path %s -> %s",
                      frNm,
                      toNm));
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
            |
              CheckMode.T.Puls =>
              EVAL CheckMargin(db,
                               arc.dataDir, arc.clkDir,
                               frTrIdx, frNm, toTrIdx, toNm,
                               MeasurePulsemargin, "pulsemargin")
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
      IF Verbose THEN
        Debug.Out(F("Measured setup from edge at %s, setup %s",
                    LR(clkTrans.at), LR(setupTime)))
      END;
      RETURN setupTime
    END
  END MeasureSetup;

PROCEDURE MeasurePulsemargin(clkIdx   : CARDINAL;
                             data     : TransitionSeq.T;
                             clk      : TransitionSeq.T;
                             dataDir  : CheckDir.T) : LONGREAL =

  (* here we are measuring the pulse margin from a flipping internal node
     to a flipping clock *)
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
    
    (* find previous transition of data -- this is the interesting transition of
       the aggressor in the case of the pulse margin *)
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

    (* check whether data transition
       was after RISING edge of clock (same cycle) *)
    
    IF dataTrans.at <= clkTrans.at THEN
      (* there were no interesting transitions on this cycle *)
      RETURN Fail
    END;

    (* we had a transition that we are sensitive to DURING the cycle *)
    WITH pulseMargin = disTrans.at - dataTrans.at DO
      IF Verbose THEN
        Debug.Out(F("Measured pulsemargin from edge at %s, setup %s",
                    LR(clkTrans.at), LR(pulseMargin)))
      END;
      RETURN pulseMargin
    END
  END MeasurePulsemargin;

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
      IF Verbose THEN
        Debug.Out(F("Measured hold from edge at %s, hold %s",
                    LR(clkTrans.at), LR(holdTime)))
      END;
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
      IF Verbose THEN
        Debug.Out(F("Measured pulse from edge at %s, pulse %s",
                    LR(clkTrans.at), LR(pulseTime)))
      END;
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

    nData := data.size();
    
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

    IF data1Idx = -1 OR data0Idx = 0 OR data0Idx = nData THEN
      RETURN Fail
    END;

    data1Trans := data.get(data1Idx);
    data0Trans := data.get(data0Idx);

    IF Verbose THEN
      Debug.Out(F("disTrans.at=%s prvTrans.at=%s data0Trans.at=%s data1Trans.at=%s",
                  LR(disTrans.at), LR(prvTrans.at),
                  LR(data0Trans.at), LR(data1Trans.at)))
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
        ret := 1.0d0/glitchWidth
      END;

      IF Verbose THEN
        Debug.Out(F("Measured glitchwidth from edge at %s, glitchwidth %s ret %s",
                    LR(clkTrans.at), LR(glitchWidth), LR(ret)))
      END;
      RETURN ret
    END
  END MeasureGlitchwidth;

TYPE Checker = PROCEDURE(clkIdx   : CARDINAL;
                         data     : TransitionSeq.T;
                         clk      : TransitionSeq.T;
                         dataDir  : CheckDir.T) : LONGREAL;

VAR nMargins := 0;

PROCEDURE GetValues(clkDirs           : CheckDir.T;
                    clkTrIdx          : CARDINAL;
                      clkNm             : TEXT;
                    thresh            : LONGREAL;
                    READONLY ta, da   : ARRAY OF LONGREAL) : CardSeq.T =
  BEGIN
    WITH res = NEW(CardSeq.T).init(),
         clkTrans = TransitionFinder.FilterTime(
                            tranFinder.forNode(clkTrIdx),
                            resetTime, LAST(LONGREAL)) DO
      Debug.Out(F("GetValues : clk  %s : %s active transitions",
                  clkNm, Int(clkTrans.size())));

      FOR i := 0 TO clkTrans.size() - 1 DO
        WITH ct = clkTrans.get(i) DO
          IF ct.dir IN clkDirs THEN
            WITH t = ct.at,
                 dtidx = FindIdxBefore(ta, t),
                 dval = da[dtidx],
                 dlogic = dval > thresh DO
              IF dlogic THEN
                res.addhi(1)
              ELSE
                res.addhi(0)
              END
            END
          END
        END
      END;
      RETURN res
    END
  END GetValues;

PROCEDURE FindIdxBefore(READONLY a : ARRAY OF LONGREAL;
                        tgt        : LONGREAL) : CARDINAL =
  VAR
    lo := 0;
    hi := NUMBER(a);
  BEGIN
    WHILE lo < hi DO
      WITH i  = lo + (hi - lo) DIV 2,
           ai = a[i] DO
        IF tgt = ai THEN
          RETURN i
        ELSIF tgt > ai THEN
          lo := i + 1
        ELSE
          hi := i
        END
      END
    END;
    <*ASSERT lo = hi*>
    WITH res = lo - 1 DO
      <*ASSERT res <= LAST(a)*>

      <*ASSERT a[res] <= tgt*>

      <*ASSERT res = LAST(a) OR a[res + 1] >= tgt*>
      (*note that a[res + 1] > tgt is more correct, but would depend
        on making an assumption on the contents of a*)
      
      RETURN res
    END
  END FindIdxBefore;
                    
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
    minMargin   := LAST(LONGREAL);
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

VAR
  mappedNames := NEW(TextTextTbl.Default).init();
  (* we remember the original names here, whatever they may be *)
     
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
      WITH mapped = mapper(t) DO
        IF NOT TE(mapped, t) THEN
          EVAL mappedNames.put(mapped, t)
        END;
        EVAL newF.put(mapped, c)
      END
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

PROCEDURE UnmapName(nm : TEXT) : TEXT =
  (* for mapped name find trace name *)
  BEGIN
    EVAL mappedNames.get(nm, nm);
    RETURN nm
  END UnmapName;

VAR tagCnts := NEW(TextIntTbl.Default).init();
    
PROCEDURE GetNextCounter(tag : TEXT) : CARDINAL =
  VAR
    cnt := 0;
    nxt : INTEGER;
  BEGIN
    EVAL tagCnts.get(tag, cnt);
    nxt := cnt + 1;
    EVAL tagCnts.put(tag, nxt);
    RETURN cnt
  END GetNextCounter;
  
PROCEDURE GraphMeasurement(meas : MarginMeasurement.T;
                           ns   : LONGREAL;
                           idx  : CARDINAL;
                           root : Pathname.T
                           ) =
  VAR
    scenStr := MarginMeasurement.Format(meas);
    fr := UnmapName(meas.scenario.datNm);
    to := UnmapName(meas.scenario.clkNm);
    wr := NEW(TextWr.T).init();
    stdout, stderr := ProcUtils.WriteHere(wr);
    wx := Wx.New();
    cmd := F("/nfs/site/disks/zsc3_fin_data_share/rliu68/tools/bin/fulcrum aplot %s", root);
    loNsF := meas.at * 1.0d9 - 0.5d0 * ns;
    hiNsF := meas.at * 1.0d9 + 0.5d0 * ns;
    loNs := ROUND(loNsF);
    hiNs := ROUND(hiNsF);
    input : TEXT;

    iWr : Wr.T;
    
  BEGIN

    Wx.PutText(wx, F("\\set title \"%s\"\n", scenStr));
    Wx.PutText(wx, F("add %s\n", fr));
    Wx.PutText(wx, F("add %s\n", to));
    Wx.PutText(wx, F("range %s:%s\n", Int(loNs), Int(hiNs)));
    Wx.PutText(wx, "\\set term png size 2000,480\n");
    Wx.PutText(wx, F("\\set output \"worst%s.%s.png\"\n",
                     meas.scenario.tag, Int(GetNextCounter(meas.scenario.tag))));
    Wx.PutText(wx, "update\n");
    Wx.PutText(wx, "quit\n");

    input := Wx.ToText(wx);
         
    Debug.Out("GraphMeasurement: " & cmd);
    Debug.Out("GraphMeasurement: input:\n" & input);


    
    WITH     reader = ProcUtils.GimmeWr(iWr),
             cm    = ProcUtils.RunText(cmd,
                                   stdout := stdout,
                                   stderr := stderr,
                                   stdin := reader)
     DO
      Wr.PutText(iWr, input);
      Wr.Close(iWr);
      
      TRY
        cm.wait()
      EXCEPT
        ProcUtils.ErrorExit(err) =>
        WITH msg = F("command \"%s\" with output\n====>\n%s\n<====\n\nraised ErrorExit : %s",
                     cmd,
                     TextWr.ToText(wr),
                     ProcUtils.FormatError(err)) DO
          Debug.Warning(msg)

        END
      END
    END
  END GraphMeasurement;

PROCEDURE MeasureFromSpice(spiceFn : Pathname.T) =
  BEGIN
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
        
        DoOneLatchSpec(db, spice, rootCkt, LatchSpecs[i], mapper)
      END;
      
      WITH worst = MarginDump.Do(db, 10) DO
        IF graphNs > 0.0d0 THEN
          FOR i := 0 TO worst.size() - 1 DO
            GraphMeasurement(worst.get(i), graphNs, i, traceRt)
          END
        END
      END
    END
  END MeasureFromSpice;

TYPE
  LatchNodes = RECORD
    clk, d, q : TEXT;
  END;

PROCEDURE IsQNode(trace          : Trace.T;
                      idx            : CARDINAL;
                      VAR latchNodes : LatchNodes) : BOOLEAN =

  PROCEDURE AliasHasSuffix(idx : CARDINAL;
                           sfx : TEXT;
                           VAR pfx : TEXT) : BOOLEAN=
    VAR iter := trace.getAliases(idx).iterate();
        a : TEXT;
    BEGIN
      WHILE iter.next(a) DO
        IF FALSE THEN
          Debug.Out(F("seeking suffix %s in %s", sfx, a))
        END;
        IF CitTextUtils.HaveSuffix(a, sfx) THEN
          pfx := Text.Sub(a, 0, Text.Length(a) - Text.Length(sfx));
          RETURN TRUE
        END
      END;
      RETURN FALSE
    END AliasHasSuffix;

  PROCEDURE Exists(nn : TEXT) : BOOLEAN =
    VAR
      dummy : CARDINAL;
    BEGIN
      RETURN trace.getNodeIdx(nn, dummy)
    END Exists;

  VAR
    pfx : TEXT;
  BEGIN
    IF AliasHasSuffix(idx, ".Q", pfx) THEN
      WITH clkNm = pfx & ".CLK",
           dNm   = pfx & ".D",
           qNm   = pfx & ".Q" DO
        
        IF Exists(pfx & ".D") AND Exists(pfx & ".CLK") THEN
          latchNodes := LatchNodes { clkNm, dNm, qNm };
          RETURN TRUE
        END
      END
    END;
    RETURN FALSE
  END IsQNode;

PROCEDURE WriteCardSeq(wr : Wr.T;
                       seq : CardSeq.T;
                       delim : TEXT;
                       truncAt := LAST(CARDINAL))
  RAISES { Wr.Failure } =
  VAR
    n := MIN(truncAt, seq.size());
  BEGIN
    FOR i := 0 TO n - 1 DO
      Wr.PutText(wr, Int(seq.get(i)));
      IF i # n - 1 THEN
        Wr.PutText(wr, delim)
      END
    END
  END WriteCardSeq;
  
PROCEDURE MeasureByName(truncValues : CARDINAL) =
  VAR
    latchNodes : LatchNodes;
    clkTrIdx, dTrIdx : CARDINAL;
    db := NEW(MarginMeasurementSeq.T).init();
    nclks := 0;
    nsteps := trace.getSteps();
    tima, data := NEW(REF ARRAY OF LONGREAL, nsteps);
    vwr := FileWr.Open("values.dat");
  BEGIN
    trace.getTimeData(tima^);
    FOR i := 0 TO trace.getNodes() - 1 DO
      IF IsQNode(trace, i, latchNodes) THEN
        INC(nclks);
        Debug.Out(F("MeasureByName : clock node %s", latchNodes.clk));

        WITH hadIt = trace.getNodeIdx(latchNodes.clk, clkTrIdx) DO
          <*ASSERT hadIt*>
        END;
        WITH hadIt = trace.getNodeIdx(latchNodes.d, dTrIdx) DO
          <*ASSERT hadIt*>
        END;
        EVAL CheckMargin(db,
                         CheckDir.All, +1,
                         dTrIdx, latchNodes.d,
                         clkTrIdx, latchNodes.clk,
                         MeasureSetup, "setup");

        EVAL CheckMargin(db,
                         UD, +1,
                         dTrIdx, latchNodes.d, (* ignored *)
                         clkTrIdx, latchNodes.clk,
                         MeasurePulsewidth, "pulsewidth-pos");

        EVAL CheckMargin(db,
                         UD, +1,
                         dTrIdx, latchNodes.d,
                         clkTrIdx, latchNodes.clk,
                         MeasureHold, "hold");

        trace.getNodeData(dTrIdx, data^);
        
        WITH seq = GetValues(Dn,
                             clkTrIdx,
                             latchNodes.clk,
                             vdd / 2.0d0,
                             tima^, data^
          ) DO
          Wr.PutText(vwr, latchNodes.d);
          Wr.PutChar(vwr, ' ');
          Wr.PutText(vwr, valueTag);
          Wr.PutText(vwr, " : ");
          WriteCardSeq(vwr, seq, delim := "", truncAt := truncValues);
          Wr.PutChar(vwr, '\n')
        END

      END
    END;

    Wr.Close(vwr);

    Debug.Out(F("%s clocks", Int(nclks)));

    WITH worst = MarginDump.Do(db, 10) DO
      IF graphNs > 0.0d0 THEN
        FOR i := 0 TO worst.size() - 1 DO
          GraphMeasurement(worst.get(i), graphNs, i, traceRt)
        END
      END
    END
    
  END MeasureByName;
  
VAR
  pp                          := NEW(ParseParams.T).init(Stdio.stderr);
  spiceFn    : Pathname.T     ; (*:= "xa.sp";*)
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
  graphNs                     := FIRST(LONGREAL);
  truncValues                 := LAST(CARDINAL);
  mapper     : Mapper;
  doByName   : BOOLEAN;
  valueTag                    := "";
  
BEGIN
  Debug.AddWarnStream(warnWr);
  
  TRY
    quick := pp.keywordPresent("-quick");

    doByName := pp.keywordPresent("-byname");

    IF pp.keywordPresent("-truncvalues") THEN
      truncValues := pp.getNextInt()
    END;
    IF pp.keywordPresent("-valuetag") THEN
      valueTag := pp.getNext()
    END;
    
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
    IF pp.keywordPresent("-graph") THEN
      graphNs := pp.getNextLongReal()
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
    mapper := RemoveX;
    MapTraceNames(trace, mapper)
  ELSE
    mapper := NIL
  END;

  IF spiceFn # NIL THEN
    MeasureFromSpice(spiceFn)
  END;

  IF doByName THEN
    MeasureByName(truncValues)
  END

END Main.
