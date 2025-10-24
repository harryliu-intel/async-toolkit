(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE ClockGen EXPORTS Main;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT Params;
IMPORT OSError, Rd;
IMPORT TransitionFinder;
IMPORT Trace;
IMPORT TraceFile;
IMPORT Debug;
FROM Fmt IMPORT F, Int, LongReal, FN;
IMPORT AL;
IMPORT Thread;
IMPORT Transition;
IMPORT TransitionSeq;
FROM TechLookup IMPORT Lookup;
IMPORT Pathname;
IMPORT FS;
IMPORT CitTextUtils;
IMPORT FileWr;
IMPORT Wr;
IMPORT Wx;
IMPORT Math;
IMPORT TextTextTbl;
IMPORT Env;
IMPORT TechTemplate;
IMPORT ProcUtils;
FROM TechCleanup IMPORT DeleteMatching, DeleteRecursively,
                        CompressFile;
IMPORT Word;
IMPORT TimingCheckers;
IMPORT CheckDir;

<*FATAL Thread.Alerted*>
<*FATAL OSError.E, Rd.Failure, Wr.Failure, Rd.EndOfFile*>


CONST
  Usage = "";

  LR = LongReal;

  SrcPath     = "spice/clock_gen4_latch/src"; (* path to src dir of this program *)
 
  Files       = ARRAY OF Pathname.T {
  "circuit.sp"
  };

TYPE
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

TYPE
  LatchArc = RECORD
    d, clk : TEXT;
  END;

CONST
  MyArcs = ARRAY OF LatchArc {
  LatchArc { "clk4[0]", "rfck" },
  LatchArc { "clk4[3]", "rfck" },
  LatchArc { "clk2[0]", "_rfck" },
  LatchArc { "clk2[1]", "_rfck" },
  LatchArc { "clk2[2]", "_rfck" },
  LatchArc { "clk2[3]", "_rfck" }
  };

CONST DutName = "u_clkgen";
  
TYPE
  ResultCol = { dl0, pw0, dl1, pw1, dl2, pw2, dl3, pw3, minh, mins };
  
  Result = ARRAY ResultCol OF LONGREAL;

CONST
  FirstResult = Result { FIRST(LONGREAL), .. };
  LastResult  = Result { LAST(LONGREAL) , .. };

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
        res[2 * i + 2]     := sdev
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
    ClkNm  = "REFCLK";
    
  VAR
    clkId := GetNode     (trace, ClkNm);
    tranFinder := NEW(TransitionFinder.T).init(trace, vdd / 2.0d0, vdd / 10.0d0);
    clkSeq : TransitionSeq.T;
    result : Result;
  BEGIN
    Debug.Out("Found node ids");

    clkSeq := tranFinder.forNode(clkId, TRUE);
    FOR i := 0 TO clkSeq.size() - 1 DO
      Debug.Out("Clock transition " & Transition.Format(clkSeq.get(i)))
    END;


    FOR i := 0 TO nphases - 1 DO
      WITH nm  = F("CLKOUT[%s]", Int(i)),
           id  = GetNode (trace, nm),
           seq = tranFinder.forNode(id, TRUE),
           clkcnt = 2 * i + 1 (* # of falling edge *),
           clkTran = clkSeq.get(clkcnt),
           seqTran0 = seq.get(0),
           seqTran1 = seq.get(1),

           latency = seqTran0.at - clkTran.at,
           pw      = seqTran1.at - seqTran0.at DO

        result[VAL(2*i    ,ResultCol)] := latency;
        result[VAL(2*i + 1,ResultCol)] := pw
      END
    END;

    (* let's measure the setup and hold timings *)

    VAR 
      minHold := LAST(LONGREAL);
      minSetup := LAST(LONGREAL);
      arcHold, arcSetup : LONGREAL;
    BEGIN
      FOR i := FIRST(MyArcs) TO LAST(MyArcs) DO
        arcHold := LAST(LONGREAL);
        arcSetup := LAST(LONGREAL);
        WITH arc   = MyArcs[i],
             dnm   = DutName & "." & arc.d,
             dId   = GetNode (trace, dnm),
             dseq  = tranFinder.forNode(dId, TRUE),
             
             clknm = DutName & "." & arc.clk,
             clkId = GetNode (trace, clknm),
             cseq  = tranFinder.forNode(clkId, TRUE) DO
          
          FOR j := 0 TO cseq.size() - 1 DO
            WITH ctran = cseq.get(j) DO
              IF ctran.dir = 1 THEN
                WITH thisSetup = TimingCheckers.MeasureSetup(j,
                                                             dseq,
                                                             cseq,
                                                             CheckDir.All),
                     thisHold  = TimingCheckers.MeasureHold(j,
                                                            dseq,
                                                            cseq,
                                                            CheckDir.All) DO
                  Debug.Out(F("clock tran %s at %s, data %s, hold %s setup %s",
                              clknm,
                              LR(ctran.at),
                              dnm,
                              LR(thisHold),
                              LR(thisSetup)));
                              
                  arcHold := MIN(arcHold, thisHold);
                  arcSetup := MIN(arcSetup, thisSetup);
                END
              END
            END
          END;
          
          Debug.Out(F("Arc %s -> %s hold %s setup %s",
                      dnm, clknm, LR(arcHold), LR(arcSetup)))
          
        END;
        minHold := MIN(minHold, arcHold);
        minSetup := MIN(minSetup, arcSetup);
      END;

      Debug.Out(F("Worst timing : hold %s setup %s", LR(minHold), LR(minSetup)));
      result[ResultCol.minh] := minHold;
      result[ResultCol.mins] := minSetup
    END;

    IF mWr # NIL THEN
      Wr.PutText(mWr,
                 Concat(",",TA{
      tag,
      traceRt,
      LR(vdd),
      LR(temp),
      LR(rise),
      process,
      Int(speed)},
      FmtLRA(result)^));

      Wr.PutChar(mWr, '\n');
    END;
    RETURN result
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
    min   := LastResult;
    max   := FirstResult;
  BEGIN
    IF measureFn # NIL THEN
      mWr := FileWr.Open(measureFn)
    END;
    IF traceRt # NIL THEN
      WITH this = DoTrace(traceRt, mWr) DO
        Accumulate(this, n, sum, sumSq, min, max)
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
              Accumulate(this, n, sum, sumSq, min, max)
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
                             FmtLRA(LRA { vdd, temp, rise })^,
                             TA { process, Int(speed) },
                             FmtLRA(DoStats(n, sum, sumSq)^)^,
                             FmtLRA(min)^,
                             FmtLRA(max)^));
      Wr.PutChar(mWr, '\n');
      Wr.Close(mWr)
    END;
  END DoPost;

PROCEDURE Accumulate(READONLY x       : Result;
                     VAR            n : LONGREAL;
                     VAR sum, sumSq   : Result;
                     VAR min, max     : Result) =
  BEGIN
    n        := n        + 1.0d0;
    FOR c := FIRST(Result) TO LAST(Result) DO
      sum  [c] := sum  [c] + x[c];
      sumSq[c] := sumSq[c] + x[c] * x[c];

      min[c]   := MIN(min[c], x[c]);
      max[c]   := MAX(max[c], x[c]);
    END
  END Accumulate;

PROCEDURE DoPre() =
  VAR
    map := NEW(TextTextTbl.Default).init();

  BEGIN
    
    Debug.Out("DoPre()");

    EVAL map.put("@CYCLE@", LR(cycle));
    
    EVAL map.put("@HSP_DIR@","/nfs/site/disks/zsc9_fwr_sd_001/mnystroe/p1278_3x0p9eu1/2023ww43d5/models_core_hspice/m14_2x_1xa_1xb_6ya_2yb_2yc__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp");
    EVAL map.put("@VDD@", LR(vdd));
    EVAL map.put("@TEMP@", LR(temp));
    
    EVAL map.put("@SWEEPS@", Int(sweeps));
    
    EVAL map.put("@PROCESS@", process);
    EVAL map.put("@RISE@", LR(rise));

    VAR
      hspiceFile : TEXT;
      pwCount    := PwCount[cell];
      ifSpec     := "";
      speedStr := "";
      val : TEXT;
    BEGIN
      CASE cell OF
        Cell.Latch =>
        hspiceFile := "CLOCK_GEN4_LATCH_ROUTED_100C.hspice";

        FOR i := 0 TO pwCount - 1 DO
          IF Word.And(Word.Shift(speed, i), 1) = 1 THEN
            val := "vtrue"
          ELSE
            val := "0"
          END;
          
          speedStr := speedStr & F("Vpwdly%s PW_DLY[%s] 0 DC=%s\n",
                                   Int(i), Int(i), val);
        END;

        ifSpec := "CLKOUT[0] CLKOUT[1] CLKOUT[2] CLKOUT[3] vssx PW_DLY[0] PW_DLY[1] PW_DLY[2] PW_DLY[3] REFCLK vcc RSTB"


      |
        Cell.LatchTherm =>
        hspiceFile := "CLOCK_GEN4_LATCH_THERMOMETER_ROUTED_100C.hspice";

        FOR i := 0 TO pwCount - 1 DO
          IF speed > i THEN
            val := "vtrue"
          ELSE
            val := "0"
          END;
          
          speedStr := speedStr & F("Vpwdly%s PW_DLY[%s] 0 DC=%s\n",
                                   Int(i), Int(i), val);

        END;
        ifSpec := "CLKOUT[0] CLKOUT[1] CLKOUT[2] CLKOUT[3] vssx PW_DLY[0] PW_DLY[10] PW_DLY[11] PW_DLY[12] PW_DLY[13] PW_DLY[14] PW_DLY[15] PW_DLY[1] PW_DLY[2] PW_DLY[3] PW_DLY[4] PW_DLY[5] PW_DLY[6] PW_DLY[7] PW_DLY[8] PW_DLY[9] REFCLK vcc RSTB"

      |
        Cell.LatchOnehot =>
        hspiceFile := "CLOCK_GEN4_LATCH_ONEHOT_ROUTED_100C.hspice";

        FOR i := 0 TO pwCount - 1 DO
          IF speed = i THEN
            val := "vtrue"
          ELSE
            val := "0"
          END;
          
          speedStr := speedStr & F("Vpwdly%s PW_DLY[%s] 0 DC=%s\n",
                                   Int(i), Int(i), val);
        END;

        ifSpec := "CLKOUT[0] CLKOUT[1] CLKOUT[2] CLKOUT[3] PW_DLY[0] PW_DLY[10] PW_DLY[11] PW_DLY[12] PW_DLY[13] PW_DLY[14] PW_DLY[1] PW_DLY[2] PW_DLY[3] PW_DLY[4] PW_DLY[5] PW_DLY[6] PW_DLY[7] PW_DLY[8] PW_DLY[9] REFCLK RSTB vcc vssx"

      END;

      EVAL map.put("@HSPICE_FILE@", hspiceFile);
      EVAL map.put("@SPEED@", speedStr);
      EVAL map.put("@IF_SPEC@", ifSpec)
    END;
    
    (********************* done setting up map *********************)
    
    FOR i := FIRST(Files) TO LAST(Files) DO
      WITH path = m3utils & "/" & SrcPath & "/" & Files[i] & ".tmpl",
           tmpl = TechTemplate.LoadTemplate(path) DO
        TechTemplate.ModifyTemplate(tmpl, map);
        TechTemplate.WriteTemplate (tmpl, Files[i])
      END
    END
  END DoPre;

PROCEDURE DoSim() =
  CONST
    SimPath = "/p/hdk/cad/hspice/U-2023.03-SP2/hspice/bin/hspice";
  VAR
    cmd            := F("%s -case 1 -mt 4 -i %s", SimPath, "circuit.sp");
    stdout, stderr := ProcUtils.WriteHere(Stdio.stderr);
    cm             := ProcUtils.RunText(cmd,
                                        stdout := stdout,
                                        stderr := stderr,
                                        stdin  := NIL);
  BEGIN
    TRY
      cm.wait()
    EXCEPT
      ProcUtils.ErrorExit(err) =>
      Debug.Error(F("Couldn't run simulator (%s) : %s", cmd, ProcUtils.FormatError(err)))
    END
  END DoSim;

PROCEDURE Convert1(fsdbRoot : Pathname.T) =
  CONST
    CtPath  = "spice/ct/AMD64_LINUX/ct";
    SzPath  = "spice/spicecompress/spicestream/AMD64_LINUX/spicestream";
    NrPath  = "spice/fsdb/src/nanosimrd";
  VAR
    Ct     := m3utils & "/" & CtPath;
    Sz     := m3utils & "/" & SzPath;
    Nr     := m3utils & "/" & NrPath;
    cmd    := FN("%s -fsdb %s -threads 4 -R 5e-12 -compress %s -format CompressedV1 -translate -wd %s.ctwork %s.fsdb %s",
                TA { Ct, Nr, Sz, fsdbRoot, fsdbRoot, fsdbRoot });
    
    stdout, stderr := ProcUtils.WriteHere(Stdio.stderr);
    cm             := ProcUtils.RunText(cmd,
                                        stdout := stdout,
                                        stderr := stderr,
                                        stdin  := NIL);
  BEGIN
    TRY
      cm.wait()
    EXCEPT
      ProcUtils.ErrorExit(err) =>
      Debug.Error(F("Couldn't run convert1 (%s) : %s", cmd, ProcUtils.FormatError(err)))
    END
  END Convert1;
  
PROCEDURE DoConv() =
  VAR
    iter := FS.Iterate(".");
    fn   : Pathname.T;
  BEGIN
    WHILE iter.next(fn) DO
      fn := CitTextUtils.CheckSuffix(fn, ".fsdb");
      IF fn # NIL THEN
        Convert1(fn)
      END
    END
  END DoConv;

PROCEDURE DoClean() =
  VAR
    iter := FS.Iterate(".");
    fn   : Pathname.T;
  BEGIN
    DeleteMatching(".", "\\.fsdb$");
    DeleteMatching(".", "\\.ic0$");
    DeleteMatching(".", "\\.mc0$");
    WHILE iter.next(fn) DO
      IF CitTextUtils.HaveSuffix(fn, ".ctwork") THEN
        DeleteRecursively(".", fn)
      END
    END;
    FOR f := FIRST(Files) TO LAST(Files) DO
      CompressFile(Files[f])
    END
  END DoClean;
  
TYPE
  Phase = { Pre, Sim, Conv, Clean, Post };
  Proc  = PROCEDURE();
  Cell  = { Latch, LatchTherm, LatchOnehot };
  
CONST
  PhaseNames = ARRAY Phase OF TEXT       { "pre", "sim", "conv", "clean", "post" };
  PhaseProc  = ARRAY Phase OF Proc       { DoPre, DoSim, DoConv, DoClean, DoPost };
  DefSweeps  = 4;
  DefSpeed   = 0;
  DefRise    = 30.0d-12;
  DefCycle   = 10000.0d-12; (* 10 ns *)

  CellNames = ARRAY Cell OF TEXT     { "latch", "latch_therm", "latch_onehot" };
  PwCount   = ARRAY Cell OF CARDINAL { 4, 16, 15 };
  
VAR
  pp                          := NEW(ParseParams.T).init(Stdio.stderr);
  vdd, temp                   := FIRST(LONGREAL);
  traceRt    : TEXT;
  phases                      := SET OF Phase { FIRST(Phase) .. LAST(Phase) };
  measureFn  : Pathname.T     := "measure.dat";
  tag        : TEXT           := "";
  sweeps     : CARDINAL       := DefSweeps;
  m3utils                     := Env.Get("M3UTILS");
  speed      : CARDINAL       := DefSpeed;

  nphases    : CARDINAL       := 4;
  
  process                     := "tttt";

  rise                        := DefRise;

  cell                        := Cell.Latch;

  cycle                       := DefCycle;
  
BEGIN
  IF m3utils = NIL THEN
    Debug.Error("?must set M3UTILS")
  END;
  
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

    IF pp.keywordPresent("-rise") THEN
      rise := pp.getNextLongReal()
    END; 

    IF pp.keywordPresent("-sweeps") THEN
      sweeps := pp.getNextInt()
    END; 

    IF pp.keywordPresent("-measurefn") OR pp.keywordPresent("-m") THEN
      measureFn := pp.getNext()
    END;

    IF pp.keywordPresent("-cell") THEN
      cell := VAL(Lookup(pp.getNext(), CellNames), Cell)
    END;

    IF pp.keywordPresent("-speed") THEN
      speed := pp.getNextInt()
    END;

    IF pp.keywordPresent("-process") THEN
      process := pp.getNext()
    END;

    IF pp.keywordPresent("-cycle") THEN
      cycle := pp.getNextLongReal()
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
    IF phase IN phases THEN
      Debug.Out("================   PHASE " & PhaseNames[phase]);
      PhaseProc[phase]()
    END
  END


END ClockGen.
