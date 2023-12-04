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
IMPORT Scan;
IMPORT Math;
IMPORT TextTextTbl;

<*FATAL Thread.Alerted*>

CONST
  Usage = "";
  TE = Text.Equal;

  Width = 32;
  LR = LongReal;

  Cells = ARRAY OF TEXT {
  "i0maboi22aa1n02x7", "i0mand002aa1n03x5", "i0maoai13aa1n02x5",
  "i0maoai13aa1n02x7", "i0maoai13aa1n03x5", "i0maob012aa1n02x5",
  "i0maob012aa1n03x5", "i0maoi012aa1n02x7", "i0maoi012aa1n03x5",
  "i0maoi013aa1n03x5", "i0maoi112aa1n02x7", "i0maoi112aa1n03x4",
  "i0maoi122aa1n02x7", "i0maoib12aa1n03x5", "i0mfun400aa1q02x6",
  "i0mfun400aa1q03x6", "i0minv000aa1n03x5", "i0mmtn022aa1n03x5",
  "i0mnanb02aa1n02x5", "i0mnanb02aa1n03x5", "i0mnand02aa1n02x5",
  "i0mnano22aa1n02x5", "i0mnanp02aa1n03x5", "i0mnona22aa1n03x5",
  "i0mnona23aa1n03x5", "i0mnona32aa1n03x5", "i0mnor002aa1n02x5",
  "i0mnorb02aa1n02x5", "i0mnorb02aa1n02x7", "i0mnorb02aa1n03x4",
  "i0mnorp02aa1n03x5", "i0moa0012aa1n03x5", "i0moab012aa1n02x5",
  "i0moab012aa1n03x5", "i0moabi12aa1n02x7", "i0moai012aa1n02x5",
  "i0moai012aa1n02x7", "i0moai012aa1n03x5", "i0moai112aa1n02x7",
  "i0moai112aa1n03x5", "i0moai122aa1n03x5", "i0moaib12aa1n02x7",
  "i0moaih12aa1n02x5", "i0morn002aa1n02x7", "i0morn002aa1n03x5",
  "i0mxnrc02aa1n03x5", "i0mxobna2aa1n03x5", "i0mxorc02aa1n03x5"
  };

  Strengths = ARRAY OF TEXT { "02x5", "02x6", "02x7", "03x4", "03x5", "03x6" };

  Pdk = "pdk080_r4v2p0_efv";
  MetalCorner = "100c_tttt_cmax";
  
TYPE
  Array = ARRAY [ 0 .. Width - 1 ] OF Trace.NodeId;
  TA    = ARRAY OF TEXT;
  LRA   = ARRAY OF LONGREAL;

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

TYPE
  ResultCol = { latency, swiE, leaP };
  
  Result = ARRAY ResultCol OF LONGREAL;

PROCEDURE FmtLRA(READONLY a : ARRAY OF LONGREAL) : REF ARRAY OF TEXT =
  VAR
    res := NEW(REF ARRAY OF TEXT, NUMBER(a));
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      res[i] := LR(a[i])
    END;
    RETURN res
  END FmtLRA;

PROCEDURE DoStats(READONLY n          : LONGREAL;
                  READONLY sum, sumSq : ARRAY OF LONGREAL)
  : REF ARRAY OF LONGREAL =
  VAR
    res := NEW(REF ARRAY OF LONGREAL, 2 * NUMBER(sum) + 1);
  BEGIN
    <* ASSERT NUMBER(sum) = NUMBER(sumSq) *>
    res[0] := n;
    FOR i := FIRST(sum) TO LAST(sum) DO
      WITH s  = sum[i],
           ss = sumSq[i],

           mean = s/n,
           var  = ss / n - mean*mean,
           sdev = Math.sqrt(n / (n - 1.0d0) * var) DO

        Debug.Out("s      = " & LR(s));
        Debug.Out("ss     = " & LR(ss));
        Debug.Out("mean   = " & LR(mean));
        Debug.Out("var    = " & LR(var));
        Debug.Out("sdev   = " & LR(sdev));
        
        res[2 * i + 1]     := mean;
        res[2 * i + 2] := sdev
      END
    END;
    RETURN res
  END DoStats;
  
PROCEDURE DoTrace(traceRt : Pathname.T; mWr : Wr.T) : Result =
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
            Wr.PutChar(mWr, '\n');

          END;
          RETURN Result { latency, swiE, leaP }

        END
      END
    END
  END
END DoTrace;

PROCEDURE Concat(sep : TEXT; READONLY lst0, lst1, lst2, lst3, lst4 := TA {}) : TEXT =

  PROCEDURE DoOne(READONLY lst : TA) =
    BEGIN
      FOR i := FIRST(lst) TO LAST(lst) DO
        IF NOT first THEN
          Wx.PutText(wx, sep)
        ELSE
          first := FALSE
        END;

        Wx.PutText(wx, lst[i]);
      END
    END DoOne;
    
  VAR
    wx    := Wx.New();
    first := TRUE;
  BEGIN
    DoOne(lst0);
    DoOne(lst1);
    DoOne(lst2);
    DoOne(lst3);
    DoOne(lst4);
    RETURN Wx.ToText(wx)
  END Concat;

PROCEDURE DoPost() =
  VAR
    mWr : Wr.T;
    n     := 0.0d0;
    sum   := Result { 0.0d0, .. };
    sumSq := sum;
    
  BEGIN
    IF measureFn # NIL THEN
      mWr := FileWr.Open(measureFn)
    END;
    IF traceRt # NIL THEN
      WITH this = DoTrace(traceRt, mWr) DO
        Accumulate(this, n, sum, sumSq)
      END
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
            WITH this = DoTrace(fn, mWr) DO
              Accumulate(this, n, sum, sumSq)
            END
          END
        END
      END
    END;
    IF measureFn # NIL THEN
      Wr.Close(mWr)
    END;
    
    IF measureFn # NIL THEN
      mWr := FileWr.Open(measureFn & ".stat");
      Wr.PutText(mWr, Concat(",",
                             FmtLRA(LRA { vdd, temp })^,
                             FmtLRA(DoStats(n, sum, sumSq)^)^));
      Wr.PutChar(mWr, '\n');
      Wr.Close(mWr)
    END;
  END DoPost;

PROCEDURE Accumulate(READONLY x : Result;
                     VAR n : LONGREAL;
                     VAR sum, sumSq : Result) =
  BEGIN
    n        := n        + 1.0d0;
    FOR c := FIRST(Result) TO LAST(Result) DO
      sum  [c] := sum  [c] + x[c];
      sumSq[c] := sumSq[c] + x[c] * x[c]
    END
  END Accumulate;

PROCEDURE WriteSource(wr         : Wr.T;
                      nm         : TEXT;
                      val        : Word.T;
                      bits       : CARDINAL;
                      src0, src1 : TEXT) =
  VAR
    src : TEXT;
  BEGIN
    FOR i := 0 TO bits - 1 DO
      WITH is  = Int(i),
           bit = Word.Extract(val, i, 1) DO
        (* compiler bug? : *)
        (*      src = ARRAY [0..1] OF TEXT { src0, src1 }[bit] *)

        CASE bit OF
          0 => src := src0
        |
          1 => src := src1
        ELSE
          <*ASSERT FALSE*>
        END;
        <*ASSERT nm  # NIL*>
        <*ASSERT is  # NIL*>
        <*ASSERT src # NIL*>
        Wr.PutText(wr, F("Vsrc__%s__%s %s %s[%s] 0\n", nm, is, src, nm, is))
      END
    END
  END WriteSource;
  
PROCEDURE DoPre() =
  CONST
    rise = 10.0d-12;
  VAR
    map := NEW(TextTextTbl.Default).init();
  BEGIN
    Debug.Out("DoPre()");
    
    WITH wr = FileWr.Open("sources.sp") DO
      WriteSource(wr, "a_i", aInput, Width, "vssx", "vcc1");
      WriteSource(wr, "b_i", bInput, Width, "vssx", "vcc1");
      Wr.Close(wr)
    END;

    EVAL map.put("@STEP@", LR(step));
    EVAL map.put("@RISE@", LR(rise));

    EVAL map.put("@HSP_DIR@","/nfs/site/disks/zsc9_fwr_sd_001/mnystroe/p1278_3x0p9eu1/2023ww43d5/models_core_hspice/m14_2x_1xa_1xb_6ya_2yb_2yc__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp");
    EVAL map.put("@VDD@", LR(vdd));
    EVAL map.put("@TEMP@", LR(temp));

    MapCells(map);

    EVAL map.put("@PDK@", Pdk);
    EVAL map.put("@METALCORNER@", MetalCorner);
    EVAL map.put("@STDCELLDIR@", LibPaths[lib]);

  END DoPre;

PROCEDURE MapCells(map : TextTextTbl.T) =
  CONST
    Def = "i0m";
  BEGIN
    FOR i := FIRST(Cells) TO LAST(Cells) DO
      WITH cn = Cells[i],
           cp = "@" & cn & "@" DO
        EVAL map.put(cp, CitTextUtils.ReplacePrefix(cn, Def, LibNames[lib]))
      END
    END
  END MapCells;

PROCEDURE DoSim() =
  BEGIN
  END DoSim;
  
TYPE
  Phase = { Pre, Sim, Post };
  Proc = PROCEDURE();
  Lib   = { I0s, I0m };

CONST
  PhaseNames = ARRAY Phase OF TEXT       { "pre", "sim", "post" };
  PhaseProc  = ARRAY Phase OF Proc       { DoPre, DoSim, DoPost };
  LibNames   = ARRAY Lib   OF TEXT       { "i0s", "i0m"  };
  LibPaths   = ARRAY Lib   OF Pathname.T { "lib783_i0s_160h_50pp", "lib783_i0m_180h_50pp" };

  DefStep    = 10.0d-9;
  DefSweeps  = 4;
  
VAR
  pp                          := NEW(ParseParams.T).init(Stdio.stderr);
  vdd, temp                   := FIRST(LONGREAL);
  traceRt    : TEXT;
  step       : LONGREAL       := DefStep;
  phases                      := SET OF Phase { };
  measureFn  : Pathname.T     := NIL;
  tag        : TEXT           := "";
  aInput, bInput : Word.T;
  lib        : Lib;
  sweeps     : CARDINAL       := DefSweeps;
  
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
    IF pp.keywordPresent("-sweeps") THEN
      sweeps := pp.getNextInt()
    END; 
    IF pp.keywordPresent("-step") THEN
      step := pp.getNextLongReal()
    END;
    IF pp.keywordPresent("-measurefn") OR pp.keywordPresent("-m") THEN
      measureFn := pp.getNext()
    END;
    IF pp.keywordPresent("-a") THEN
      aInput := Scan.Unsigned(pp.getNext())
    END;
    IF pp.keywordPresent("-b") THEN
      bInput := Scan.Unsigned(pp.getNext())
    END;
    IF pp.keywordPresent("-lib") THEN
      lib := VAL(Lookup(pp.getNext(), LibNames),
                                           Lib)
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
