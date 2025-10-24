(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE RingOsc EXPORTS Main;
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
IMPORT Text;
IMPORT Transition;
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
IMPORT Env;
IMPORT TechTemplate;
IMPORT ProcUtils;
FROM TechCleanup IMPORT DeleteMatching, DeleteRecursively,
                        CompressFilesWithExtension,  CompressFile;
IMPORT FileRd;
IMPORT RefSeq;
IMPORT SpeedSample;
IMPORT SpeedSampleSeq;
IMPORT SpeedSampleArraySort;

<*FATAL Thread.Alerted*>
<*FATAL OSError.E, Rd.Failure, Wr.Failure, Rd.EndOfFile*>


CONST
  Usage = "";
  TE = Text.Equal;


  LR = LongReal;

  Pdk         = "pdk080_r4v2p0_efv";
  MetalCorner = "100c_tttt_cmax";

  SrcPath     = "spice/ringosc/src"; (* path to src dir of this program *)
 
  Files       = ARRAY OF Pathname.T {
  "circuit.sp",
  "include.sp"
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
  ResultCol = { cycletime, swiE, leaP, minCyc, maxCyc, maxStep };
  
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
    ClkNm  = "ck";
    INm    = "i(vvcc)";
    
  VAR
    clkId := GetNode     (trace, ClkNm);
    iId   := GetNode     (trace, INm  );
    tranFinder := NEW(TransitionFinder.T).init(trace, vdd / 2.0d0, vdd / 10.0d0);
    clkSeq : TransitionSeq.T;
    slowName : TEXT;
    minCyc, maxCyc, maxStep := LAST(LONGREAL);
  BEGIN
    Debug.Out("Found node ids");

    clkSeq := tranFinder.forNode(clkId, TRUE);
    FOR i := 0 TO clkSeq.size() - 1 DO
      Debug.Out("Clock transition " & Transition.Format(clkSeq.get(i)))
    END;

    IF doSweep THEN
      (* find relevant clock transitions *)
      VAR
        samples := NEW(SpeedSampleSeq.T).init();
      BEGIN
        FOR i := 0 TO maxSpeed DO
          Debug.Out("Working on speed " & Int(i));
          
          WITH speedSeq = TransitionFinder.FilterTime(clkSeq,
                                                      SpeedStart(i),
                                                      SpeedStart(i + 1)),
               riseSeq  = TransitionFinder.FilterDir(speedSeq, +1)
           DO
            
            IF riseSeq.size() < 5 THEN
              Debug.Error("?not enough transitions at speed " & Int(i))
            END;
            
            WITH start   = riseSeq.get(2),
                 mid     = riseSeq.get(3),
                 stop    = riseSeq.get(4),
                 cyctime = 0.5d0 * (stop.at - start.at) ,
                 
                 cyc0    = mid.at  - start.at,
                 cyc1    = stop.at - mid.at,
                 delcyctime = cyc1 - cyc0,
                 
                 freq    = 1.0d0 / cyctime,
                 sample  = SpeedSample.T {
                                          speed := i,
                                          beg := start, end := stop,
                                          begT := start.at, endT := stop.at,
                                          cyc := cyctime,
                                          freq := freq,
                                          delcyc := delcyctime,
                                          pctFromPrev := FIRST(LONGREAL)}  DO
              Debug.Out(F("speed %s start %s stop %s cycle %s speed %s",
                          Int(i),
                          LR(start.at), LR(stop.at), LR(cyctime), LR(freq)));
              samples.addhi (sample)
            END
          END
        END(*ROF*);

        WITH fast = samples.get(0),
             slow = samples.get(samples.size() - 1),
             fastMult = slow.cyc / fast.cyc DO
          Debug.Out("Fast speed " & SpeedSample.Format(fast));
          Debug.Out("Slow speed " & SpeedSample.Format(slow));
          Debug.Out("Tap " & LR (fastMult));

          minCyc := fast.cyc;
          maxCyc := slow.cyc
         
        END;

        WITH a = NEW(REF ARRAY OF SpeedSample.T, samples.size()) DO
          FOR i := FIRST(a^) TO LAST(a^) DO
            a[i] := samples.get(i)
          END;
          
          FOR i := 1 TO LAST(a^) DO
            WITH cur = a[i],
                 prv = a[i-1] DO
              cur.pctFromPrev := (cur.cyc - prv.cyc) / prv.cyc
            END
          END;

          WITH wr = FileWr.Open(traceRt & ".speeds") DO
            FOR i := FIRST(a^) TO LAST(a^) DO
              WITH s = a[i] DO
                Wr.PutText(wr, F("%s,%s,%s\n", Int(s.speed), LR(s.cyc), LR(s.pctFromPrev)))
              END
            END(*ROF*);
            Wr.Close(wr)
          END;

          SpeedSampleArraySort.Sort(a^);
          WITH smallStep = a[1].pctFromPrev,
               bigStep   = a[LAST(a^)].pctFromPrev,
               relTap  = bigStep / smallStep DO
            Debug.Out("big   step " & LR(bigStep));
            Debug.Out("small step " & LR(smallStep));
            Debug.Out("ratio      " & LR(relTap));

            maxStep := bigStep
          END
        END
      END
    END(*doSweep FI*);

    WITH clk4 = clkSeq.get(4),
         clkBeg = clk4.at,
         clk6 = clkSeq.get(6),
         clkEnd = clk6.at,
         cycletime = clkEnd - clkBeg,

         idleEnd = 9.0d-9,
         
         ns        = trace.getSteps(),
         timea     = NEW(REF ARRAY OF LONGREAL, ns),
         dataa     = NEW(REF ARRAY OF LONGREAL, ns) DO
      
      Debug.Out("clk4 transition is " & Transition.Format(clk4));
      Debug.Out("clk6 transition is " & Transition.Format(clk6));
      Debug.Out("cycletime " & LR(cycletime));

      IF doCalibrate THEN
        WITH wr = FileWr.Open("calibrate.dat") DO
          Wr.PutText(wr, LR(cycletime));
          Wr.PutChar(wr, '\n');
          Wr.Close(wr)
        END
      END;

      Debug.Out(F("clkBeg    = %s",  LR(clkBeg)));        
      Debug.Out(F("clkEnd    = %s",  LR(clkEnd)));        
      Debug.Out(F("idleEnd   = %s",  LR(idleEnd)));
      
      trace.getTimeData(timea^);
      trace.getNodeData(iId, dataa^);
      
      WITH swiMean = -Trace.MeanValue(timea^, dataa^, clkBeg   , clkEnd),
           leaMean = -Trace.MeanValue(timea^, dataa^, idleEnd / 2.0d0, idleEnd),
           
           swiTime = clkEnd - clkBeg,
           
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
          LR(vdd),
          LR(temp),
          LR(cycletime),
          LR(swiE),
          LR(leaP),
          LR(minCyc),
          LR(maxCyc),
          LR(maxStep),
          slowName
          }));
          Wr.PutChar(mWr, '\n');
          
        END;
        RETURN Result { cycletime, swiE, leaP, minCyc, maxCyc, maxStep }
        
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
    min   := LastResult;
    max   := FirstResult;
  BEGIN
    WITH settingsPerTap        = ninterp * settings,
         taps                  = stages - 1,
         totalSpeeds             = taps * settingsPerTap + 1 DO
      maxSpeed := MIN(maxSpeed, totalSpeeds - 1);
    END;
      
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
                             FmtLRA(LRA { vdd, temp })^,
                             TA { },
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


TYPE
  Settings = RECORD
    coarseCode : REF ARRAY OF [0..1];
    interpCode : REF ARRAY OF CARDINAL;
  END;

PROCEDURE FmtCoarse(READONLY a : ARRAY OF [0..1]) : TEXT =
  VAR
    res := "";
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      CASE a[i] OF
        0 => res := res & "0"
      |
        1 => res := res & "1"
      END
    END;
    RETURN res
  END FmtCoarse;

PROCEDURE ToSettings(speed : CARDINAL) : Settings =
  VAR
    settingsPerTap        := ninterp * settings;
    res := Settings { coarseCode := NEW(REF ARRAY OF [0..1],   stages),
                      interpCode := NEW(REF ARRAY OF CARDINAL, ninterp) };
    tapLo      := speed DIV settingsPerTap;
    tapHi      := tapLo + 1;
    speedInTap := speed MOD settingsPerTap;
    
    codeInTap : CARDINAL; (* actual code *)
    debugStr     := "";
    taps                  := stages - 1;
    totalSpeeds             := taps * settingsPerTap + 1;
  BEGIN
    IF speed > totalSpeeds - 1 THEN
      Debug.Error(F("?error in ToSettings : speed %s > %s", Int(speed), Int(totalSpeeds - 1)))
    END;

    (* if tap is odd, we have to reverse the code *)
    IF tapLo MOD 2 = 0 THEN
      codeInTap := speedInTap
    ELSE
      codeInTap := settingsPerTap - speedInTap
    END;
    
    FOR i := FIRST(res.interpCode^) TO LAST(res.interpCode^) DO
      IF    codeInTap DIV settings > i THEN
        res.interpCode[i] := settings
      ELSIF codeInTap DIV settings = i THEN
        res.interpCode[i] := codeInTap MOD settings
      ELSE
        res.interpCode[i] := 0
      END;
      
      debugStr := debugStr & Int(res.interpCode[i]) & " ";
    END;

    FOR i := 0 TO stages - 1 DO
      IF i = tapLo OR i = tapHi THEN
        res.coarseCode[i] := 1
      ELSE
        res.coarseCode[i] := 0
      END
    END;

    Debug.Out(F("speed %4s, coarse %s, interpCode %s",
                FmtCoarse(res.coarseCode^),
                Int(speed),
                debugStr));

    RETURN res
   
  END ToSettings;

PROCEDURE DumpSequentialSources(to            : Pathname.T;
                                READONLY sarr : ARRAY OF Settings)
  RAISES { OSError.E, Wr.Failure } =

  VAR
    lastVal : BOOLEAN;
    
  PROCEDURE Init(startVal : BOOLEAN) =
    BEGIN
      lastVal := startVal;
    END Init;
    
  PROCEDURE Emit(wx : Wx.T; at : LONGREAL; value : BOOLEAN) =
    BEGIN
      IF value # lastVal THEN
        VAR
          onm, nnm : TEXT;
        BEGIN
          IF value THEN
            onm := "0";
            nnm := "vtrue"
          ELSE
            onm := "vtrue";
            nnm := "0"
          END;

          Wx.PutText(wx, F("%s %s %s %s ",
                           LR(at - 0.5d0 * Rise),
                           onm,
                           LR(at + 0.5d0 * Rise),
                           nnm))
        END;
        lastVal := value
      END
    END Emit;
    
  CONST
    Rise      = 2.0d-12;
  VAR
    wr       := FileWr.Open(to);
    fineWx   := Wx.New();
    coarseWx := Wx.New();

  BEGIN
    (* fine settings *)
    FOR i := 0 TO ninterp - 1 DO
      FOR j := 0 TO settings - 1 DO
        WITH s = Int(i),
             t = Int(j) DO
          Wx.PutText(fineWx, F("Vf%s_%s f%s_%s 0 DC=0 PWL 0 0 ", t, s, t, s));

          (* put data *)

          Init(FALSE);

          FOR k := FIRST(sarr) TO LAST(sarr) DO
            WITH startTime = SpeedStart(k),
                 val       = sarr[k].interpCode[i] DO
              IF val > j THEN
                Emit(fineWx, startTime, TRUE)
              ELSE
                Emit(fineWx, startTime, FALSE)
              END
            END
          END;

          Wx.PutChar(fineWx, '\n')


        END
      END
    END;

    (* coarse settings *)
    VAR
      pow : TEXT;
      nam : TEXT;
    BEGIN
      FOR i := 0 TO stages - 1 DO
        CASE i MOD 2 OF
          0 => nam := F("ev%s", Int(i DIV 2))
        |
          1 => nam := F("od%s", Int(i DIV 2))
        END;
        Wx.PutText(coarseWx, F("V%s %s 0 DC=0 PWL 0 0 ", nam, nam, pow));
        
        (* put data *)

        Init(FALSE);
        
        FOR k := FIRST(sarr) TO LAST(sarr) DO
          WITH startTime = SpeedStart(k),
               val       = sarr[k].coarseCode[i] DO
            IF val = 0 THEN
              Emit(coarseWx, startTime, FALSE)
            ELSE
              Emit(coarseWx, startTime, TRUE)
            END
          END
        END;

        Wx.PutChar(coarseWx, '\n')
            
      END
    END;

    Wr.PutText(wr, Wx.ToText(coarseWx));
    Wr.PutText(wr, Wx.ToText(fineWx));
    
    Wr.Close(wr)
  END DumpSequentialSources;
  
PROCEDURE DoPre() =
  CONST
    rise = 10.0d-12;
  VAR
    map := NEW(TextTextTbl.Default).init();
    settingsPerTap        := ninterp * settings;
    taps                  := stages - 1;
    totalSpeeds             := taps * settingsPerTap + 1;
    fullSimEnd : LONGREAL;

  BEGIN
    
    maxSpeed := MIN(maxSpeed, totalSpeeds - 1);

    fullSimEnd := SpeedStart(maxSpeed + 3); (* some overrun *)
    
    Debug.Out("DoPre()");

    Debug.Out(FN("ninterp %s, settings %s, settingsPerTap %s, taps %s, totalSpeeds %s, speed %s",
                 TA { Int(ninterp), Int(settings), Int(settingsPerTap), Int(taps), Int(totalSpeeds), Int(speed) }));

    IF speed > totalSpeeds - 1 THEN
      Debug.Error(F("?error speed %s > %s", Int(speed), Int(totalSpeeds - 1)))
    END;

    WITH sarr = NEW(REF ARRAY OF Settings, totalSpeeds) DO
      FOR i := 0 TO totalSpeeds - 1 DO
        sarr[i] := ToSettings(i)
      END;

      IF doSweep THEN
        DumpSequentialSources("settings.sp", sarr^)
      ELSE
        WITH wr = FileWr.Open("settings.sp") DO
          Wr.Close(wr)
        END
      END
    END;
    
    VAR
      tapLo      := speed DIV settingsPerTap;
      speedInTap := speed MOD settingsPerTap;
      tapHi      := tapLo + 1;

      codeInTap : CARDINAL; (* actual code *)
      
      interpCode   := NEW(REF ARRAY OF CARDINAL, ninterp); (* per interpolator *)

      debugStr     := "";
      
    BEGIN
      (* if tap is odd, we have to reverse the code *)
      IF tapLo MOD 2 = 0 THEN
        codeInTap := speedInTap
      ELSE
        codeInTap := settingsPerTap - speedInTap
      END;

      FOR i := FIRST(interpCode^) TO LAST(interpCode^) DO
        IF    codeInTap DIV settings > i THEN
          interpCode[i] := settings
        ELSIF codeInTap DIV settings = i THEN
          interpCode[i] := codeInTap MOD settings
        ELSE
          interpCode[i] := 0
        END;

        debugStr := debugStr & Int(interpCode[i]) & " ";
      END;

      Debug.Out("interpCode : " & debugStr);

      (* note that for the SLOWEST speed (only), tapHi will 
         refer to a block that does not exist *)

      WITH interpWx = Wx.New(),
           fineWx   = Wx.New(),
           allfineWx= Wx.New() DO
        FOR i := 0 TO ninterp - 1 DO
          WITH s = Int(i) DO
            Wx.PutText(interpWx, F("Xinterp%s e eb o ob ", s));

            FOR j := 0 TO settings - 1 DO
              WITH t = Int(j) DO
                Wx.PutText(interpWx, F("f%s_%s ", t, s));
                Wx.PutText(allfineWx, F("f%s_%s ", t, s));

                VAR
                  pow : TEXT;
                BEGIN
                  IF interpCode[i] > j THEN
                    pow := "vcc"
                  ELSE
                    pow := "vssx"
                  END;
                  Wx.PutText(fineWx, F("Vf%s_%s f%s_%s %s 0\n", t, s, t, s, pow))
                END
              END
            END;

            Wx.PutText(interpWx, "ck ckb vcc vssx interp\n")
          END
        END;
        
        EVAL map.put("@INTERPOLATOR@", Wx.ToText(interpWx));
        EVAL map.put("@FINEMAXSPEED@", Wx.ToText(fineWx));
        EVAL map.put("@ALLFINE@"     , Wx.ToText(allfineWx))
      END;

      IF doCalibrate THEN
        EVAL map.put("@AUTOSTOP@", ".OPTION AUTOSTOP");
      ELSE
        EVAL map.put("@AUTOSTOP@", "");
      END;
      
      VAR
        coarseWx := Wx.New();
        pow : TEXT;
        nam : TEXT;
      BEGIN
        FOR i := 0 TO stages - 1 DO
          CASE i MOD 2 OF
            0 => nam := F("ev%s", Int(i DIV 2))
          |
            1 => nam := F("od%s", Int(i DIV 2))
          END;
          IF i = tapLo OR i = tapHi OR (tapHi = taps AND i = taps - 2) THEN
            pow := "rstb"
          ELSE
            pow := "vssx"
          END;
          Wx.PutText(coarseWx, F("V%s %s %s 0\n", nam, nam, pow))
        END;
        EVAL map.put("@COARSEMAXSPEED@", Wx.ToText(coarseWx))
      END;

      IF doSweep THEN
        EVAL map.put("@COARSEMAXSPEED@", "");
        EVAL map.put("@FINEMAXSPEED@"  , "")
      END;
      
      EVAL map.put("@RISE@", LR(rise));
      
      EVAL map.put("@HSP_DIR@","/nfs/site/disks/zsc9_fwr_sd_001/mnystroe/p1278_3x0p9eu1/2023ww43d5/models_core_hspice/m14_2x_1xa_1xb_6ya_2yb_2yc__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp");
      EVAL map.put("@VDD@", LR(vdd));
      EVAL map.put("@TEMP@", LR(temp));
      
      EVAL map.put("@PDK@", Pdk);
      EVAL map.put("@METALCORNER@", MetalCorner);
      EVAL map.put("@SWEEPS@", Int(sweeps));
      IF doCalibrate THEN
        EVAL map.put("@SIMTIME@", "1")
      ELSE
        EVAL map.put("@SIMTIME@", LR(fullSimEnd))
      END;

      EVAL map.put("@PROCESS@", process);
      
      (* done setting up map *)
      
      FOR i := FIRST(Files) TO LAST(Files) DO
        WITH path = m3utils & "/" & SrcPath & "/" & Files[i] & ".tmpl",
             tmpl = TechTemplate.LoadTemplate(path) DO
          TechTemplate.ModifyTemplate(tmpl, map);
          TechTemplate.WriteTemplate (tmpl, Files[i])
        END
      END
    END
  END DoPre;

PROCEDURE DoSim() =
  CONST
    SimPath = "/p/hdk/cad/hspice/U-2023.03-SP2/hspice/bin/hspice";
  VAR
    cmd            := F("%s -mt 4 -i %s", SimPath, "circuit.sp");
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

PROCEDURE SpeedStart(speed : CARDINAL) : LONGREAL =
  (* when to start simulating the given speed *)
  BEGIN
    IF speed = 0 THEN
      RETURN resetTime
    ELSE
      RETURN SpeedStart(speed - 1) +
             baseTimeAtSpeed + multTimeAtSpeed * FLOAT(speed, LONGREAL)
    END
  END SpeedStart;

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
  Proc = PROCEDURE();

CONST
  PhaseNames = ARRAY Phase OF TEXT       { "pre", "sim", "conv", "clean", "post" };
  PhaseProc  = ARRAY Phase OF Proc       { DoPre, DoSim, DoConv, DoClean, DoPost };
  DefSweeps  = 4;
  DefNinterp  = 8;
  DefSpeed   = 0;
  DefStages  = 6;
  DefSettings= 4;

  DefBaseTimeAtSpeed =  1.0d-9;  (* one ns base *)
  DefMultTimeAtSpeed = 25.0d-12; (* 25 ps increase per setting *)
  DefResetTime       = 10.0d-9;  (* 10 ns for reset *)
  
VAR
  pp                          := NEW(ParseParams.T).init(Stdio.stderr);
  vdd, temp                   := FIRST(LONGREAL);
  traceRt    : TEXT;
  phases                      := SET OF Phase { FIRST(Phase) .. LAST(Phase) };
  measureFn  : Pathname.T     := "measure.dat";
  tag        : TEXT           := "";
  sweeps     : CARDINAL       := DefSweeps;
  m3utils                     := Env.Get("M3UTILS");
  ninterp    : CARDINAL       := DefNinterp;
  speed      : CARDINAL       := DefSpeed;
  stages     : CARDINAL       := DefStages;
  settings   : CARDINAL       := DefSettings;

  baseTimeAtSpeed             := DefBaseTimeAtSpeed;
  multTimeAtSpeed             := DefMultTimeAtSpeed;

  resetTime                   := DefResetTime;
  doSweep    : BOOLEAN;
  maxSpeed   : CARDINAL       := LAST(CARDINAL);
  doCalibrate                 := FALSE;
  calibFn    : Pathname.T     := NIL;
  process                     := "tttt";
  
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
    IF pp.keywordPresent("-basetimeatspeed") THEN
      baseTimeAtSpeed := pp.getNextLongReal()
    END; 
    IF pp.keywordPresent("-multtimeatspeed") THEN
      multTimeAtSpeed := pp.getNextLongReal()
    END; 
    IF pp.keywordPresent("-calibfile") THEN
      calibFn := pp.getNext()
    END;
    IF pp.keywordPresent("-temp") THEN
      temp := pp.getNextLongReal()
    END; 
    IF pp.keywordPresent("-sweeps") THEN
      sweeps := pp.getNextInt()
    END; 
    IF pp.keywordPresent("-stages") THEN
      stages := pp.getNextInt()
    END; 
    IF pp.keywordPresent("-ninterp") THEN
      ninterp := pp.getNextInt()
    END; 
    IF pp.keywordPresent("-measurefn") OR pp.keywordPresent("-m") THEN
      measureFn := pp.getNext()
    END;

    IF pp.keywordPresent("-sweepspeeds") THEN
      doSweep := TRUE;
      IF pp.keywordPresent("-maxspeed") THEN
        maxSpeed := pp.getNextInt()
      END
    ELSIF pp.keywordPresent("-speed") THEN
      speed := pp.getNextInt();
      IF pp.keywordPresent("-calibrate") THEN
        doCalibrate := TRUE
      END
    END;

    IF pp.keywordPresent("-process") THEN
      process := pp.getNext()
    END;

    IF calibFn # NIL THEN
      WITH rd      = FileRd.Open(calibFn),
           line    = Rd.GetLine(rd),
           calcyc  = Scan.LongReal(line) DO
        baseTimeAtSpeed := 20.0d0 * calcyc;
        multTimeAtSpeed := calcyc * 0.1d0;
      END
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


END RingOsc.
