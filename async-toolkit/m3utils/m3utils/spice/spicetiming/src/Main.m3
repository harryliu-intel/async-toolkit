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
FROM Fmt IMPORT F, Int;
IMPORT Trace;
IMPORT RegEx;
IMPORT SpiceCircuit;
IMPORT SpiceObject;
IMPORT Text;
IMPORT TraceRep;
IMPORT CitTextUtils;
IMPORT TextCardTbl;
IMPORT TextSeq;
IMPORT TransitionFinder;
IMPORT Transition;

CONST
  Usage = "";
  TE = Text.Equal;


CONST
  Up = CheckDir { 1 };
  Dn = CheckDir { -1 };
  UD = CheckDir { -1, 1 };

TYPE
  CheckDir = SET OF Transition.Dir;

  CheckMode = { Setu, Hold };
  
  Arc = RECORD
    from, to : TEXT;
    mode     : CheckMode;
    dir      : CheckDir;
  END;

  ArcArr = ARRAY [ 0 .. MaxArcs - 1 ] OF Arc;
  
  LatchSpec = RECORD
    typeNamePattern : TEXT;
    arcs            : ArcArr;
  END;

CONST
  MaxArcs = 4;
  NoArc   = Arc { NIL, NIL, FIRST(CheckMode), Dn };

  VendorLatch = LatchSpec {
  "vendor_D_intel_D_g1i_D_l",
  ArcArr {
    Arc { "d", "clk", CheckMode.Setu, UD },
    Arc { "d", "clk", CheckMode.Hold, UD },
    NoArc,
    NoArc
  }};

  TinyLatch = LatchSpec {
  "D_TINY_U_LATCH_D_a.v.",
  ArcArr {
    Arc { "D",    "CLK", CheckMode.Setu, Up },
    Arc { "D", "_U_CLK", CheckMode.Setu, Dn },
    Arc { "D",    "CLK", CheckMode.Hold, Up },
    Arc { "D", "_U_CLK", CheckMode.Hold, Dn }
  }};
  
  LatchSpecs = ARRAY OF LatchSpec { VendorLatch, TinyLatch };

PROCEDURE DoOneLatchSpec(sf             : SpiceFormat.T;
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
              CheckLatch(x, subCkt, spec.arcs, trace) 
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

PROCEDURE CheckLatch(x               : SpiceObject.X; (* instantiation *)
                     subCkt          : SpiceCircuit.T;(* of what *)
                     READONLY arcs   : ARRAY OF Arc;  (* ...validate against *)
                     trace           : Trace.T
  ) =
  VAR
    fromTrIdx, toTrIdx : CARDINAL;
  BEGIN
    Debug.Out(F("Checking latch %s of type %s", x.name, subCkt.name));
    
    FOR i := FIRST(arcs) TO LAST(arcs) DO
      IF arcs[i].from # NIL THEN
        WITH arc     = arcs[i],
             fromIdx = FindFormalIdx(subCkt, arc.from),
             toIdx   = FindFormalIdx(subCkt, arc.to),
             fromNm  = x.terminals.get(fromIdx),
             toNm    = x.terminals.get(toIdx),
             hadFromTr = trace.getNodeIdx(fromNm, fromTrIdx),
             hadToTr   = trace.getNodeIdx(toNm, toTrIdx) DO
          Debug.Out(F("verifying path %s (%s) -> %s (%s)",
                      fromNm, Int(fromIdx),
                      toNm, Int(toIdx)));
          IF NOT hadFromTr AND NOT hadToTr THEN
            Debug.Warning(F("Can't find from,to nodes %s,%s in trace",
                            fromNm, toNm))
          ELSIF NOT hadFromTr THEN
            Debug.Warning(F("Can't find from node %s in trace",
                            fromNm))
          ELSIF NOT hadToTr THEN
            Debug.Warning(F("Can't find to node %s in trace", toNm))
          ELSE
            FOR dir := FIRST(Transition.Dir) TO LAST(Transition.Dir) DO
              IF dir IN arcs[i].dir THEN
                CheckArc(dir, fromTrIdx, fromNm, toTrIdx, toNm, arc.mode)
              END
            END
          END
        END
      END
    END
  END CheckLatch;

PROCEDURE CheckArc(dir : Transition.Dir;
                   frTrIdx : CARDINAL;
                   frNm : TEXT;
                   toTrIdx : CARDINAL;
                   toNm : TEXT;
                   mode : CheckMode) =
  BEGIN
    WITH frTrans = TransitionFinder.FilterTime(
                       TransitionFinder.FilterDir(
                           tranFinder.forNode(frTrIdx),
                           dir),
                       resetTime, LAST(LONGREAL)),
         toTrans = TransitionFinder.FilterTime(
                       TransitionFinder.FilterDir(
                           tranFinder.forNode(toTrIdx),
                           dir),
                       resetTime, LAST(LONGREAL)) DO
      
      (* hmm we really should be looking for glitches here, too! *)
      
      Debug.Out(F("%s : %s transitions",
                  frNm, Int(frTrans.size())));
      Debug.Out(F("%s : %s transitions",
                  toNm, Int(toTrans.size())));
      
      (* scan toTrans (clock transitions) *)
      FOR i := 0 TO toTrans.size() - 1 DO
        WITH toTran = toTrans.get(i),
             frPrev = TransitionFinder.FindFloorIdx(frTrans, toTran.at)
         DO
          (* here toTran is a clock transition,
             frPrev is the index of the input transition last before
             toTran, or -1 if it does not exist *)
          
          
        END
      END
      
    END
  END CheckArc;
  
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
  
BEGIN
  TRY
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
  
  FOR i := FIRST(LatchSpecs) TO LAST(LatchSpecs) DO
    DoOneLatchSpec(spice, rootCkt, LatchSpecs[i])
  END;

END Main.
