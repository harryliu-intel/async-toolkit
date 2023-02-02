MODULE Main;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT Debug;
IMPORT Params;
IMPORT Pathname;
IMPORT SpiceFormat;
IMPORT AL;
IMPORT Rd, FileRd;
IMPORT OSError;
FROM Fmt IMPORT F;
IMPORT Trace;
IMPORT SpiceCircuit;
IMPORT Text;
IMPORT TraceRep;
IMPORT CitTextUtils;
IMPORT TextCardTbl;
IMPORT TextSeq;
IMPORT TransitionFinder; 
IMPORT FileWr;
IMPORT TextTextTbl;
IMPORT Wr;
IMPORT Thread;
IMPORT TextSet;
IMPORT Dsim;
IMPORT NameRefTbl;
IMPORT NameNameTbl;
IMPORT NameNameListTbl;
FROM SpiceTiming IMPORT MeasureByName, MeasureFromSpice;

<*FATAL Thread.Alerted*>

CONST
  Usage = "";
  TE = Text.Equal;

VAR nMargins := 0;

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
  resetTime                   := 0.0d-9;
  warnWr                      : Wr.T;
  quick      : BOOLEAN;
  graphNs                     := FIRST(LONGREAL);
  truncValues                 := LAST(CARDINAL);
  mapper     : Mapper;
  doByName   : BOOLEAN;
  valueTag                    := "";
  allNames   : TextSet.T;
  translate  : BOOLEAN;
  Dot        : TEXT;
  dutPfx                      := "";
  doMapTraceNames             := FALSE;
  dsimFn     : Pathname.T     := NIL;

  
BEGIN
  Debug.AddWarnStream(warnWr);
  
  TRY
    quick := pp.keywordPresent("-quick");

    translate := pp.keywordPresent("-translate");

    doMapTraceNames := pp.keywordPresent("-maptracenames");

    IF translate THEN
      Dot := "."
    ELSE
      Dot := "_D_"
    END;

    doByName := pp.keywordPresent("-byname");

    IF pp.keywordPresent("-truncvalues") THEN
      truncValues := pp.getNextInt()
    END;
    IF pp.keywordPresent("-valuetag") THEN
      valueTag := pp.getNext()
    END;

    IF pp.keywordPresent("-dutname") THEN
      dutPfx := pp.getNext() & "."
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
    IF pp.keywordPresent("-resettime") THEN
      resetTime := pp.getNextLongReal()
    END;
    IF pp.keywordPresent("-graph") THEN
      graphNs := pp.getNextLongReal()
    END;
    IF pp.keywordPresent("-dsim") THEN
      dsimFn := pp.getNext()
    END;

    pp.skipParsed();
    pp.finish()
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command-line parameters\nUsage: " & Params.Get(0) & " " & Usage)
  END;

  warnWr                      := FileWr_Open(traceRt & "_spicetiming.warn");

  IF dsimFn # NIL THEN
    VAR
      types, decls, topLevelWires : NameRefTbl.T := NEW(NameRefTbl.Default).init();
      rd := FileRd.Open(dsimFn);
      define : Dsim.Define;
      instanceTypes := NEW(NameNameTbl.Default).init();
      typeInstances := NEW(NameNameListTbl.Default).init();
    BEGIN
      define := Dsim.Parse(rd, types, decls, topLevelWires);
      Rd.Close(rd);

      Dsim.Flatten(define,
                   types,
                   instanceTypes,
                   typeInstances);
    END
  END;
  
  TRY
    trace := NEW(Trace.T).init(traceRt)
  EXCEPT
    OSError.E(x) => Debug.Error(F("Trouble opening input trace %s : OSError.E : %s", traceRt, AL.Format(x)))
  |
    Rd.Failure(x) => Debug.Error(F("Trouble reading input trace %s : Rd.Failure : %s", traceRt, AL.Format(x)))
  |
    Rd.EndOfFile =>
    Debug.Error(F("Short read opening input trace"))
  END;

  
  (* map names *)

  IF doMapTraceNames THEN
    mapper := RemoveX;
    MapTraceNames(trace, mapper)
  ELSE
    mapper := NIL
  END;

  allNames := trace.allNames();

  tranFinder := NEW(TransitionFinder.T).init(trace, vdd / 2.0d0, vdd / 10.0d0);

  IF spiceFn # NIL THEN
    MeasureFromSpice(spiceFn, quick, nMargins, trace, spice, translate, rootType, rootCkt, mapper, traceRt, graphNs, Dot, allNames, dutPfx, tranFinder, resetTime, mappedNames)
  END;

  IF doByName THEN
    MeasureByName(truncValues, trace, traceRt, vdd, valueTag, graphNs, nMargins, tranFinder, resetTime, mappedNames)
  END

END Main.
