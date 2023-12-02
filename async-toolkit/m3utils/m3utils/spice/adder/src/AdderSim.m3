MODULE AdderSim EXPORTS Main;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT Params;
IMPORT OSError, Rd;
IMPORT TransitionFinder;
IMPORT Trace;
IMPORT TraceFile;
IMPORT Debug;
FROM Fmt IMPORT F, Int, Unsigned, LongReal, FN;
IMPORT AL;
IMPORT Thread;
IMPORT Text;
IMPORT Transition;
IMPORT Word;
IMPORT V01X;
IMPORT TransitionSeq;
FROM TechLookup IMPORT Lookup;
IMPORT Pathname;
IMPORT FS;
IMPORT CitTextUtils;
IMPORT FileWr;
IMPORT Wr;
IMPORT Wx;

<*FATAL Thread.Alerted*>

CONST
  Usage = "";
  TE = Text.Equal;

  Width = 32;
  LR = LongReal;

TYPE
  Array = ARRAY [ 0 .. Width - 1 ] OF Trace.NodeId;
  TA    = ARRAY OF TEXT;

PROCEDURE GetNode(trace : Trace.T; nm : TEXT) : Trace.NodeId =
  VAR
    idx : Trace.NodeId;
  BEGIN
    WITH hadIt = trace.getNodeIdx(nm, idx) DO
      IF NOT hadIt THEN
        Debug.Error(F("Can't find node %s in names.", nm))
      END
    END;
    RETURN idx
  END GetNode;
  
PROCEDURE GetNodeArray(trace   : Trace.T;
                       nm      : TEXT;
                       VAR arr : ARRAY OF Trace.NodeId) =
  BEGIN
    FOR i := 0 TO NUMBER(arr) - 1 DO
      arr[i] := GetNode(trace, nm & "[" & Int(i) & "]")
    END
  END GetNodeArray;

PROCEDURE GetArrayValue(tFinder        : TransitionFinder.T;
                        time           : LONGREAL;
                        READONLY nodes : Array) : Word.T =
  VAR
    res : Word.T := 0;
  BEGIN
    FOR i := LAST(nodes) TO FIRST(nodes) BY -1 DO
      res := Word.LeftShift(res, 1);
      
      WITH tSeq = tFinder.forNode(nodes[i], TRUE),
           v    = TransitionFinder.FindValueAt(tSeq, time) DO
        CASE v OF
          V01X.T.V0 => (* skip *)
        |
          V01X.T.V1 => res := Word.Or(res, 1)
        |
          V01X.T.VX => RETURN LAST(Word.T)
        END
      END
    END;
    RETURN res
  END GetArrayValue;


PROCEDURE GetFinalTransition(tFinder          : TransitionFinder.T;
                             trace            : Trace.T;
                             READONLY inArray : ARRAY OF Trace.NodeId;
                             VAR slowName     : TEXT ) : Transition.T =
  VAR
    maxTime := FIRST(LONGREAL);
    maxNode : Trace.NodeId;
    maxTran : Transition.T;
  BEGIN
    FOR i := FIRST(inArray) TO LAST(inArray) DO
      WITH tSeq = tFinder.forNode(inArray[i], TRUE) DO
        IF tSeq.size() # 0 THEN
          WITH last = tSeq.get(tSeq.size() - 1) DO
            IF last.at > maxTime THEN
              maxTran := last;
              maxNode := inArray[i];
              maxTime := last.at
            END
          END
        END
      END
    END;

    slowName :=  trace.getCanonicalName(maxNode);

    Debug.Out(F("Last transition on node id %s canon %s",
                Int(maxNode),
                slowName));
               
    RETURN maxTran
  END GetFinalTransition;
  
PROCEDURE DoTrace(traceRt : Pathname.T; mWr : Wr.T;) =
  VAR
    trace      : Trace.T;
  BEGIN
    TRY
      trace := NEW(Trace.T).init(traceRt)
  EXCEPT
    OSError.E(x) => Debug.Error(F("Trouble opening input trace %s : OSError.E : %s", traceRt, AL.Format(x)))
  |
    Rd.Failure(x) => Debug.Error(F("Trouble reading input trace %s : Rd.Failure : %s", traceRt, AL.Format(x)))
  |
    Rd.EndOfFile =>
    Debug.Error(F("Short read opening input trace"))
  |
    TraceFile.FormatError =>
    Debug.Error(F("Trace file format error"))
  END;

  Debug.Out("Loaded trace");

  CONST
    AArr   = "a_i";
    BArr   = "b_i";
    YdArr  = "y_d";
    
    ClkNm  = "clk";
    INm    = "i(vvcc)";
    
  VAR
    aIds,  
    bIds,
    ydIds : Array;
    clkId := GetNode     (trace, ClkNm);
    iId   := GetNode     (trace, INm  );
    tranFinder := NEW(TransitionFinder.T).init(trace, vdd / 2.0d0, vdd / 10.0d0);
    clkSeq : TransitionSeq.T;
    slowName : TEXT;
  BEGIN
    GetNodeArray(trace, AArr , aIds);
    GetNodeArray(trace, BArr , bIds);
    GetNodeArray(trace, YdArr, ydIds);

    Debug.Out("Found node ids");
  

    clkSeq := tranFinder.forNode(clkId, TRUE);
    FOR i := 0 TO clkSeq.size() - 1 DO
      Debug.Out("Clock transition " & Transition.Format(clkSeq.get(i)))
    END;

    WITH aVal  = GetArrayValue(tranFinder, 4.0d0 * step, aIds ),
         bVal  = GetArrayValue(tranFinder, 4.0d0 * step, bIds ),
         ydVal = GetArrayValue(tranFinder, 5.0d0 * step, ydIds),
         final       = GetFinalTransition(tranFinder, trace, ydIds, slowName),
         prevClkIdx  = TransitionFinder.FindFloorIdx(clkSeq, final.at),
         prevClkTran = clkSeq.get(prevClkIdx),
         latency     = final.at - prevClkTran.at DO

      Debug.Out(F("a       =  %10s", Unsigned(aVal)));
      Debug.Out(F("b       =  %10s", Unsigned(bVal)));
      Debug.Out(F("a + b   =  %10s", Unsigned(aVal + bVal)));
      Debug.Out(F("y_d     =  %10s", Unsigned(ydVal)));

      Debug.Out("Final y_d transition is " & Transition.Format(final));
      Debug.Out("Prev  clk transition is " & Transition.Format(prevClkTran));
      Debug.Out("Latency " & LR(latency));

      WITH clkBeg    = prevClkTran.at - vdd / prevClkTran.slew (* some margin *),
           switchEnd = final.at + latency (* some margin, again *),
           idleEnd   = 6.0d0 * step,
           ns        = trace.getSteps(),
           timea     = NEW(REF ARRAY OF LONGREAL, ns),
           dataa     = NEW(REF ARRAY OF LONGREAL, ns) DO
        Debug.Out(F("clkBeg    = %s",  LR(clkBeg)));        
        Debug.Out(F("switchEnd = %s",  LR(switchEnd)));        
        Debug.Out(F("idleEnd   = %s",  LR(idleEnd)));

        trace.getTimeData(timea^);
        trace.getNodeData(iId, dataa^);

        WITH swiMean = -Trace.MeanValue(timea^, dataa^, clkBeg   , switchEnd),
             leaMean = -Trace.MeanValue(timea^, dataa^, switchEnd, idleEnd),

             swiTime = switchEnd - clkBeg,
             
             leaP    = leaMean * vdd,
             swiP    = swiMean * vdd,
             dynP    = swiP - leaP,
             
             swiE    = swiP * swiTime,
             dynE    = dynP * swiTime DO
          Debug.Out(FN("swiMean   = %s\nleaMean   = %s\nswiTime   = %s\nleaP      = %s\nswiP      = %s\ndynP      = %s\nswiE      = %s\ndynE      = %s",
                       TA{LR(swiMean),LR(leaMean),LR(swiTime),LR(leaP),LR(swiP),LR(dynP),LR(swiE),LR(dynE)}));
          IF mWr # NIL THEN
            Wr.PutText(mWr,
                       Concat(",",TA{
            tag,
            traceRt,
            Unsigned(aVal),
            Unsigned(bVal),
            Unsigned(aVal + bVal),
            Unsigned(ydVal) ,
            LR(vdd),
            LR(temp),
            LR(latency),
            LR(swiE),
            LR(leaP),
            slowName
            }));
            Wr.PutChar(mWr, '\n')
          END
        END
      END
    END
  END
END DoTrace;

PROCEDURE Concat(sep : TEXT; READONLY lst : TA) : TEXT =
  VAR
    wx := Wx.New();
  BEGIN
    FOR i := FIRST(lst) TO LAST(lst) DO
      Wx.PutText(wx, lst[i]);
      IF i # LAST(lst) THEN
        Wx.PutText(wx, sep)
      END
    END;
    RETURN Wx.ToText(wx)
  END Concat;

PROCEDURE DoPost() =
  VAR
    mWr : Wr.T;
  BEGIN
    IF measureFn # NIL THEN
      mWr := FileWr.Open(measureFn)
    END;
    IF traceRt # NIL THEN
      DoTrace(traceRt, mWr)
    ELSE
      CONST
        extension = ".names";
      VAR
        iter := FS.Iterate(".");
        fn   : Pathname.T;
      BEGIN
        WHILE iter.next(fn) DO
          fn := CitTextUtils.CheckSuffix(fn, extension);
          IF fn # NIL THEN
            DoTrace(fn, mWr)
          END
        END
      END
    END;
    IF measureFn # NIL THEN
      Wr.Close(mWr)
    END
  END DoPost;

PROCEDURE DoPre() =
  BEGIN
  END DoPre;
  
TYPE
  Phase = { Pre, Post };
  Proc = PROCEDURE();
  
CONST
  PhaseNames = ARRAY Phase OF TEXT { "pre", "post" };
  PhaseProc  = ARRAY Phase OF Proc { DoPre, DoPost };

VAR
  pp                          := NEW(ParseParams.T).init(Stdio.stderr);
  vdd, temp                   := FIRST(LONGREAL);
  traceRt    : TEXT;
  step       : LONGREAL;
  phases                      := SET OF Phase { };
  measureFn  : Pathname.T     := NIL;
  tag        : TEXT           := "";
  
BEGIN
  TRY
    IF pp.keywordPresent("-t") THEN
      traceRt := pp.getNext()
    END;
    IF pp.keywordPresent("-vdd") THEN
      vdd := pp.getNextLongReal()
    END; 
    IF pp.keywordPresent("-temp") THEN
      temp := pp.getNextLongReal()
    END; 
    IF pp.keywordPresent("-tag") THEN
      tag := pp.getNext()
    END; 
    IF pp.keywordPresent("-step") THEN
      step := pp.getNextLongReal()
    END;
    IF pp.keywordPresent("-measurefn") OR pp.keywordPresent("-m") THEN
      measureFn := pp.getNext()
    END;
    IF pp.keywordPresent("-p") THEN
      phases := SET OF Phase {};
      
      REPEAT
        phases := phases + SET OF Phase { VAL(Lookup(pp.getNext(), PhaseNames),
                                           Phase) }
      UNTIL NOT pp.keywordPresent("-p")
    END;
    pp.skipParsed();
    pp.finish()
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command-line parameters\nUsage: " & Params.Get(0) & " " & Usage)
  END;

  FOR phase := FIRST(Phase) TO LAST(Phase) DO
    IF phase IN phases THEN PhaseProc[phase]() END
  END


END AdderSim.
