MODULE SpiceTiming;
IMPORT Pathname;
IMPORT MarginMeasurementSeq;
IMPORT FileRd;
IMPORT OSError;
IMPORT SpiceFormat;
IMPORT SpiceError;
IMPORT AL;
IMPORT Debug;
FROM Fmt IMPORT F, Int, Bool;
IMPORT MarginDump;
IMPORT Wr;
IMPORT Rd;
IMPORT Thread;
IMPORT SpiceTranslate;
IMPORT FileWr;
IMPORT CheckDir;
IMPORT TransitionFinder;
IMPORT Transition;
FROM TimingCheckers IMPORT Checker, 
                           MeasureSetup, MeasurePulsemargin,
                           MeasureHold, MeasurePulsewidth, MeasureGlitchwidth;
IMPORT MarginMeasurement;
IMPORT SpiceCircuit;
IMPORT SpiceObject;
IMPORT MarginScenario;
IMPORT RegEx;
IMPORT Trace;
FROM Latches IMPORT LatchNodes, IsQNode;
IMPORT CardSeq;
FROM SpiceValues IMPORT GetValues;
IMPORT ProcUtils;
IMPORT TextWr;
IMPORT Wx;
FROM LatchSpecs IMPORT Specs;
IMPORT Text;
IMPORT TextSet;
IMPORT CheckMode;
IMPORT TextTextTbl;
IMPORT TextIntTbl;

<*FATAL Thread.Alerted*>

CONST TE = Text.Equal;

VAR Verbose := Debug.DebugThis("SpiceTiming");

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

PROCEDURE FindHierName1(instance : SpiceObject.T;
                       subCkt   : SpiceCircuit.T;
                       node     : Node;
                       mapper   : Mapper;
                       Dot      : TEXT) : TEXT =
  BEGIN
    IF node.internal THEN
      WITH tent = instance.name & Dot & node.nm DO
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
  END FindHierName1;

PROCEDURE UnX(txt : TEXT) : TEXT =
  BEGIN
    IF Text.GetChar(txt, 0) = 'X' THEN
      RETURN Text.Sub(txt, 1)
    ELSE
      RETURN txt
    END
  END UnX;

PROCEDURE Try(nm : TEXT; allNames : TextSet.T) : BOOLEAN =
  BEGIN
    WITH res = allNames.member(nm) DO
      Debug.Out(F("Trying %s, found = %s", nm, Bool(res)));
      RETURN res
    END
  END Try;
  
PROCEDURE FindHierName(instance : SpiceObject.T;
                       subCkt   : SpiceCircuit.T;
                       node     : Node;
                       mapper   : Mapper;
                       Dot      : TEXT;
                       allNames : TextSet.T;
                       dutPfx   : TEXT) : TEXT =
  BEGIN
    WITH hn = FindHierName1(instance, subCkt, node, mapper, Dot) DO
      WITH tryName = dutPfx & hn DO
        IF Try(tryName, allNames) THEN RETURN tryName END
      END;
      WITH tryName = dutPfx & UnX(hn) DO
        IF Try(tryName, allNames) THEN RETURN tryName END
      END
    END;

    WITH name2 = instance.name & Dot & node.nm DO
      WITH tryName = dutPfx & name2 DO
        IF Try(tryName, allNames) THEN RETURN tryName END
      END;
      WITH tryName = dutPfx & UnX(name2) DO
        IF Try(tryName, allNames) THEN RETURN tryName END
      END;

      RETURN "UNKNOWN(" & name2 & ")"
    END
  END FindHierName;

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

PROCEDURE DoOneLatchSpec(db             : MarginMeasurementSeq.T;
                         sf             : SpiceFormat.T;
                         rootCkt        : SpiceCircuit.T;
                         READONLY spec  : LatchSpec;
                         mapper         : Mapper;
                         quick          : BOOLEAN;
                         nMargins       : CARDINAL;
                         trace          : Trace.T;
                       Dot      : TEXT;
                       allNames : TextSet.T;
                       dutPfx   : TEXT;
                      tranFinder        : TransitionFinder.T;
                      resetTime         : LONGREAL) =
  (* this is for a flattened design with one level of subcircuit remaining *)
  VAR
    regEx := RegEx.Compile(spec.typeNamePattern);
    elems := rootCkt.elements;
    subCkt : SpiceCircuit.T;
  BEGIN
    IF Verbose THEN
      Debug.Out(F("DoOneLatchSpec, elems.size=%s, subckts=%s, last=%s",
                  Int(elems.size()),
                  Int(sf.subCkts.size()),
                  sf.subCktNames.get(sf.subCktNames.size() - 1)
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
              CheckLatch(db, x, subCkt, spec.arcs, trace, mapper, Dot, allNames, dutPfx, nMargins, tranFinder, resetTime) 
            END
          END
        ELSE
            (* skip *)
        END
      END
    END
  END DoOneLatchSpec;

PROCEDURE CheckMargin(db                : MarginMeasurementSeq.T;
                      datDir            : CheckDir.T;
                      clkDir            : Transition.Dir;
                      datTrIdx          : CARDINAL;
                      datNm             : TEXT;
                      clkTrIdx          : CARDINAL;
                      clkNm             : TEXT;
                      checker           : Checker;
                      tag               : TEXT;
                      nMargins          : CARDINAL;
                      tranFinder        : TransitionFinder.T;
                      resetTime         : LONGREAL
                      ) : LONGREAL =
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

PROCEDURE FileWr_Open(fn : TEXT) : Wr.T =
  BEGIN
    TRY
      RETURN FileWr.Open(fn)
    EXCEPT
      OSError.E(x) =>
      Debug.Error(F("Trouble opening %s for writing : OSError.E : %s",
                    fn, AL.Format(x)));
      <*ASSERT FALSE*>
    END
  END FileWr_Open;
  
PROCEDURE MeasureByName(truncValues : CARDINAL;
                        trace       : Trace.T;
                        traceRt     : TEXT;
                        vdd         : LONGREAL;
                        valueTag    : TEXT;
                        graphNs     : LONGREAL;
                      nMargins          : CARDINAL;
                      tranFinder        : TransitionFinder.T;
                      resetTime         : LONGREAL;
                    mappedNames : TextTextTbl.T) =
  VAR
    latchNodes       : LatchNodes;
    clkTrIdx, dTrIdx : CARDINAL;
    
    db         := NEW(MarginMeasurementSeq.T).init();
    nclks      := 0;
    nsteps     := trace.getSteps();
    tima, data := NEW(REF ARRAY OF LONGREAL, nsteps);
    vwr        := FileWr_Open(traceRt & "_values.dat");
    
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
                         MeasureSetup, "setup",
                         nMargins,
                         tranFinder,
                         resetTime);

        EVAL CheckMargin(db,
                         UD, +1,
                         dTrIdx, latchNodes.d, (* ignored *)
                         clkTrIdx, latchNodes.clk,
                         MeasurePulsewidth, "pulsewidth-pos",
                         nMargins,
                         tranFinder,
                         resetTime);

        EVAL CheckMargin(db,
                         UD, +1,
                         dTrIdx, latchNodes.d,
                         clkTrIdx, latchNodes.clk,
                         MeasureHold, "hold",
                         nMargins,
                         tranFinder,
                         resetTime);

        trace.getNodeData(dTrIdx, data^);

        TRY
          WITH seq = GetValues(Dn,
                               clkTrIdx,
                               latchNodes.clk,
                               vdd / 2.0d0,
                               tima^, data^,
                               tranFinder,
                               resetTime
            ) DO
            Wr.PutText(vwr, latchNodes.d);
            Wr.PutChar(vwr, ' ');
            Wr.PutText(vwr, valueTag);
            Wr.PutText(vwr, " : ");
            WriteCardSeq(vwr, seq, delim := "", truncAt := truncValues);
            Wr.PutChar(vwr, '\n')
          END
        EXCEPT
          Wr.Failure(x) =>
          Debug.Error("Write error writing values : Wr.Failure : " &
            AL.Format(x))
        END

      END
    END;

    Wr.Close(vwr);

    Debug.Out(F("%s clocks", Int(nclks)));

    WITH worst = MarginDump.Do(db, 10, traceRt) DO
      IF graphNs > 0.0d0 THEN
        FOR i := 0 TO worst.size() - 1 DO
          GraphMeasurement(worst.get(i), graphNs, i, traceRt, mappedNames)
        END
      END
    END
    
  END MeasureByName;

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

PROCEDURE MeasureFromSpice(spiceFn        : Pathname.T;
                           quick          : BOOLEAN;
                           nMargins       : CARDINAL;
                           trace          : Trace.T;
                           spice          : SpiceFormat.T;
                           translate      : BOOLEAN;
                           rootType       : TEXT;
                           rootCkt        : SpiceCircuit.T;
                           mapper         : Mapper;
                           traceRt        : Pathname.T;
                           graphNs        : LONGREAL;
                       Dot      : TEXT;
                       allNames : TextSet.T;
                       dutPfx   : TEXT;
                      tranFinder        : TransitionFinder.T;
                      resetTime         : LONGREAL;
                    mappedNames : TextTextTbl.T) =
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

    IF translate THEN
      SpiceTranslate.Translate(spice)
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
      FOR i := FIRST(Specs) TO LAST(Specs) DO
        IF quick AND nMargins >= QuickMargins THEN EXIT END;
        
        DoOneLatchSpec(db, spice, rootCkt, Specs[i], mapper, quick, nMargins, trace, Dot, allNames, dutPfx, tranFinder, resetTime)
      END;
      
      WITH worst = MarginDump.Do(db, 10, traceRt) DO
        IF graphNs > 0.0d0 THEN
          FOR i := 0 TO worst.size() - 1 DO
            GraphMeasurement(worst.get(i), graphNs, i, traceRt, mappedNames)
          END
        END
      END
    END
  END MeasureFromSpice;
    
PROCEDURE CheckLatch(db              : MarginMeasurementSeq.T;
                     x               : SpiceObject.X; (* instantiation *)
                     subCkt          : SpiceCircuit.T;(* of what *)
                     READONLY arcs   : ARRAY OF Arc;  (* ...validate against *)
                     trace           : Trace.T;
                     mapper          : Mapper;
                     Dot      : TEXT;
                     allNames : TextSet.T;
                     dutPfx   : TEXT;
                     nMargins          : CARDINAL;
                     tranFinder        : TransitionFinder.T;
                     resetTime         : LONGREAL
  ) =
  VAR
    frTrIdx, toTrIdx     : CARDINAL;
    
    posClkIdx, posDatIdx : CARDINAL;
    posClkNm , posDatNm  : TEXT;
    gotPos := FALSE;

    negClkIdx            : CARDINAL;
    negClkNm             : TEXT;
    gotNeg := FALSE;
  CONST
    UnNil = Debug.UnNil;
  BEGIN
    Debug.Out(F("Checking latch %s of type %s", x.name, subCkt.name));
    
    FOR i := FIRST(arcs) TO LAST(arcs) DO
      IF arcs[i].fr # NoNode THEN
        WITH arc     = arcs[i],
             frNm    = FindHierName(x, subCkt, arc.fr, mapper, Dot, allNames, dutPfx),
             toNm    = FindHierName(x, subCkt, arc.to, mapper, Dot, allNames, dutPfx),
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
                               MeasureSetup, "setup",
                         nMargins,
                         tranFinder,
                         resetTime)
            |
              CheckMode.T.Hold =>
              EVAL CheckMargin(db,
                               arc.dataDir, arc.clkDir,
                               frTrIdx, frNm, toTrIdx, toNm,
                               MeasureHold, "hold",
                         nMargins,
                         tranFinder,
                         resetTime)
            |
              CheckMode.T.Puls =>
              EVAL CheckMargin(db,
                               arc.dataDir, arc.clkDir,
                               frTrIdx, frNm, toTrIdx, toNm,
                               MeasurePulsemargin, "pulsemargin",
                         nMargins,
                         tranFinder,
                         resetTime)
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
                       MeasureGlitchwidth, "glitchwidth",
                         nMargins,
                         tranFinder,
                         resetTime)
    ELSE
      Debug.Warning(F("No pos clk/data for latch %s", x.name))
    END;

    (* check for clock pulse widths *)
    IF gotPos THEN
      EVAL CheckMargin(db,
                       UD, +1,
                       posDatIdx, posDatNm, (* ignored *)
                       posClkIdx, posClkNm,
                       MeasurePulsewidth, "pulsewidth-pos",
                         nMargins,
                         tranFinder,
                         resetTime)
    END;
   
    IF gotNeg THEN
      EVAL CheckMargin(db,
                       UD, -1,
                       posDatIdx, posDatNm, (* ignored *)
                       negClkIdx, negClkNm,
                       MeasurePulsewidth, "pulsewidth-neg",
                         nMargins,
                         tranFinder,
                         resetTime)
    END;
   
    

  END CheckLatch;

PROCEDURE UnmapName(nm : TEXT;
                    mappedNames : TextTextTbl.T) : TEXT =
  (* for mapped name find trace name *)
  BEGIN
    EVAL mappedNames.get(nm, nm);
    RETURN nm
  END UnmapName;

PROCEDURE GraphMeasurement(meas : MarginMeasurement.T;
                           ns   : LONGREAL;
                           idx  : CARDINAL;
                           root : Pathname.T;
                           mappedNames : TextTextTbl.T
                           ) =
  VAR
    scenStr := MarginMeasurement.Format(meas);
    fr := UnmapName(meas.scenario.datNm, mappedNames);
    to := UnmapName(meas.scenario.clkNm, mappedNames);
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

BEGIN END SpiceTiming.
