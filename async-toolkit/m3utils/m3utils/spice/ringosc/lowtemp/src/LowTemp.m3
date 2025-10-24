(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE LowTemp EXPORTS Main;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT Params;
IMPORT OSError, Rd;
IMPORT TransitionFinder;
IMPORT Trace;
IMPORT TraceFile;
IMPORT Debug;
FROM Fmt IMPORT F, Int, Unsigned, LongReal, FN, Bool, Pad;
IMPORT AL;
IMPORT Thread;
IMPORT Text;
IMPORT Transition;
IMPORT Word;
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
FROM TechConfig IMPORT Tran, TranNames;
IMPORT Simulation;
IMPORT SimulationSeq;
IMPORT Process;
IMPORT RefSeq;
IMPORT Random;

<*FATAL Thread.Alerted*>

VAR
  NbPool  := Env.Get("NBPOOL");
  NbOpts  := Env.Get("NBRUNOPTIONS");

CONST
  Usage = "";
  TE = Text.Equal;

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
  "i0mxnrc02aa1n03x5", "i0mxobna2aa1n03x5", "i0mxorc02aa1n03x5",
  "i0mxor002aa1n02x5"
  };

  Pdk         = "pdk080_r4v2p0_efv";
  MetalCorner = "100c_tttt_cmax";

  SrcPath     = "spice/ringosc/lowtemp/src"; (* path to src dir of this program *)
 
  Files       = ARRAY OF Pathname.T {
  "circuit.sp",
  "the_dut.sp",
  "include.sp"
  };

  TheRoot = "the_oscillator";
  
  TranMagic = ARRAY Tran OF TEXT { NIL, "aa", NIL, "ab", NIL, "mc", "md" };
  
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
  ResultCol = { cycle, swiE, leaP, dynE };
  
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
    OutNm  = "out";
    INm    = "i(vvcc)";
    
  VAR
    outId := GetNode     (trace, OutNm);
    iId   := GetNode     (trace, INm  );
    tranFinder := NEW(TransitionFinder.T).init(trace, vdd / 2.0d0, vdd / 10.0d0);
    outSeq : TransitionSeq.T;
  BEGIN
    Debug.Out("Found node ids");

    outSeq := tranFinder.forNode(outId, TRUE);
    FOR i := 0 TO outSeq.size() - 1 DO
      Debug.Out("Output transition " & Transition.Format(outSeq.get(i)))
    END;

    WITH ns        = trace.getSteps(),
         timea     = NEW(REF ARRAY OF LONGREAL, ns),
         dataa     = NEW(REF ARRAY OF LONGREAL, ns) DO
      
      trace.getTimeData(timea^);
      trace.getNodeData(iId, dataa^);
      
      WITH resetEnd = step - 0.5d0 * rise,

           out4  = outSeq.get(4),
           out8  = outSeq.get(8),

           cycleTime = (out8.at - out4.at) / 2.0d0,
           
           swiMean = -Trace.MeanValue(timea^, dataa^, 2.0d0 * step, 5.0d0*step),
           leaMean = -Trace.MeanValue(timea^, dataa^, 0.2d0 * step, resetEnd),
           
           leaP    = leaMean * vdd,
           swiP    = swiMean * vdd,
           dynP    = swiP - leaP,

           swiE    = swiP * cycleTime,
           dynE    = dynP * cycleTime DO

        Debug.Out(FN("swiMean   = %s\nleaMean   = %s\nleaP      = %s\nswiP      = %s\ndynP      = %s\nswiE      = %s\ndynE      = %s",
                     TA{LR(swiMean),LR(leaMean),LR(leaP),
                        LR(swiP),LR(dynP),LR(swiE),LR(dynE)}));
        
        IF mWr # NIL THEN
          Wr.PutText(mWr,
                     Concat(",",TA{
          tag,
          traceRt,
          LibNames[lib],
          TranNames[tran],
          Bool(doNominal),
          LR(vdd),
          LR(temp),
          LR(deln),
          LR(delp),
          Int(stages),
          LR(cycleTime),
          LR(swiE),
          LR(dynE),
          LR(leaP),
          LR(cscale),
          LR(rscale)
          }));
          Wr.PutChar(mWr, '\n');
          
        END;
        RETURN Result { cycleTime, swiE, leaP, dynE }
      END
    END
  END
END DoTrace;

PROCEDURE Concat(sep : TEXT;
                 READONLY lst0, lst1, lst2, lst3, lst4 := TA {}) : TEXT =

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

      PROCEDURE DoOneDir(dir : Pathname.T) =
        CONST
          extension = ".names";
        VAR
          iter := FS.Iterate(dir);   
          fn   : Pathname.T;
        BEGIN
          WHILE iter.next(fn) DO
            fn := dir & "/" & fn;
            
            fn := CitTextUtils.CheckSuffix(fn, extension);
            IF fn # NIL THEN
              WITH this = DoTrace(fn, mWr) DO
                Accumulate(this, n, sum, sumSq)
              END
            END
          END
        END DoOneDir;

      BEGIN
        FOR i := 0 TO MAX(0, batches - 1) DO
          DoOneDir(Subdir(i))
        END
      END
    END;
    IF measureFn # NIL THEN
      Wr.Close(mWr)
    END;
    
    IF measureFn # NIL THEN
      mWr := FileWr.Open(measureFn & ".stat");
      Wr.PutText(mWr, Concat(",",
                             FmtLRA(LRA { vdd, temp, cscale, rscale, deln, delp , FLOAT(stages,LONGREAL)})^,
                             TA { Bool(modLeaves) },
                             TA { LibNames[lib], TranNames[tran], Bool(doNominal) },
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

PROCEDURE Scale1(ifn, ofn : Pathname.T; modRegEx : TEXT) =
  CONST
    DecPath = "spice/decorate/AMD64_LINUX/spicedecorate";
    VthPath = "spice/decorate/src/vth.scm";
  VAR
    Decorate := m3utils & "/" & DecPath;
    cmd := FN("%s -i %s -o %s -root %s -noprobe -capmul %s -resmul %s -S %s -modify M '(modify-mos-vth %s %s)' %s",
              TA{Decorate, ifn, ofn, TheRoot, LR(cscale), LR(rscale),
                 m3utils & "/" & VthPath,
                 LR(deln), LR(delp),
                 modRegEx });
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
      Debug.Error(F("Couldn't run scale1 (%s) : %s", cmd, ProcUtils.FormatError(err)))
    END
  END Scale1;
  
CONST
  rise = 10.0d-12;

PROCEDURE DoPre() =
  VAR
    map := NEW(TextTextTbl.Default).init();
    rand := NEW(Random.Default).init();
  BEGIN
    Debug.Out("DoPre()");
    
    EVAL map.put("@STEP@", LR(step));
    EVAL map.put("@RISE@", LR(rise));
    EVAL map.put("@TRAN@", TranNames[tran]);

    EVAL map.put("@HSP_DIR@","/nfs/site/disks/zsc9_fwr_sd_001/mnystroe/p1278_3x0p9eu1/2023ww43d5/models_core_hspice/m14_2x_1xa_1xb_6ya_2yb_2yc__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp");
    EVAL map.put("@VDD@", LR(vdd));
    EVAL map.put("@TEMP@", LR(temp));

    MapCells(map);

    EVAL map.put("@PDK@", Pdk);
    EVAL map.put("@METALCORNER@", MetalCorner);
    EVAL map.put("@STDCELLDIR@", LibPaths[lib]);

    PROCEDURE FmtIdx(idx : CARDINAL) : TEXT =
      BEGIN
        RETURN Pad(Unsigned(idx), 4, '0')
      END FmtIdx;
      
    VAR
      wx := Wx.New();
    BEGIN
      FOR i := 1 TO stages - 1 DO
        WITH is   = FmtIdx(i),
             ip1s = FmtIdx((i + 1) MOD stages) DO
          Wx.PutText(wx,
                     F("X%s x%s x%s vcc vssx ring_stage\n",
                       is, is, ip1s));
          Wx.PutText(wx, F(".PROBE TRAN v(x%s)\n", is))
        END
      END;

      EVAL map.put("@THE_STAGES@", Wx.ToText(wx))
    END;

    (* create directories if need be, else set sweeps *)

    IF batches # 0 THEN
      FOR i := 0 TO batches - 1 DO
        WITH subdir = Subdir(i) DO
          TRY
            FS.CreateDirectory(subdir)
          EXCEPT
            OSError.E(x) =>
            Debug.Warning(F("couldn't create directory %s : OSError.E : %s",
                            subdir, AL.Format(x)))
          END
        END
      END
    END;
    
    (* done setting up map -- write out the files *)

    FOR d := 0 TO MAX(0, batches - 1) DO

      Process.SetWorkingDirectory(Subdir(d));

      VAR
        offset : [-1..LAST(CARDINAL)];
      BEGIN
        IF doNominal THEN
          offset := -1
        ELSIF doDeterministic THEN
          offset := 0
        ELSE
          offset := rand.integer(0, 2000 * 2000)
        END;
          
        IF batches = 0 THEN
          EVAL map.put("@SWEEPS@", F("%s FIRSTRUN=2", Int(sweeps + offset)))
        ELSE
          WITH firstSweep = externalSweep * d,
               remSweeps  = sweeps - firstSweep,
               doSweeps   = MIN(externalSweep, remSweeps) DO
            EVAL map.put("@SWEEPS@", F("%s FIRSTRUN=%s",
                                       Int(doSweeps),
                                       Int(2 + offset + externalSweep * d)))
          END
        END
      END;
      
      FOR i := FIRST(Files) TO LAST(Files) DO
        WITH path = m3utils & "/" & SrcPath & "/" & Files[i] & ".tmpl",
             tmpl = TechTemplate.LoadTemplate(path) DO
          TechTemplate.ModifyTemplate(tmpl, map);
          TechTemplate.WriteTemplate (tmpl, Files[i])
        END
      END;
      
      VAR
        modRegEx : TEXT;
      BEGIN
        IF modLeaves = TRUE THEN
          modRegEx := ""
        END;
        Scale1("the_dut.sp", "the_scaled_dut.sp", modRegEx)
      END
    END;

    Process.SetWorkingDirectory(runDir)
  END DoPre;

PROCEDURE Subdir(n : CARDINAL) : Pathname.T =
  BEGIN
    IF batches = 0 THEN
      <* ASSERT n = 0 *>
      RETURN runDir
    ELSE
      <* ASSERT n < batches *>
      RETURN F("%s/%s",
               runDir,
               Pad(Int(n), 6, padChar := '0'))
    END
  END Subdir;

PROCEDURE MapCells(map : TextTextTbl.T) =

  PROCEDURE DoTran(cn : TEXT) : TEXT =
    CONST
      Sc = 9;
      Lc = 2;
    BEGIN
      <*ASSERT TE(Text.Sub(cn, Sc, Lc), "aa")*>
      RETURN
        Text.Sub(cn, 0, Sc) & TranMagic[tran] & Text.Sub(cn, Sc + Lc)
    END DoTran;
    
  CONST
    Def = "i0m";
  BEGIN
    FOR i := FIRST(Cells) TO LAST(Cells) DO
      WITH cn = Cells[i],
           cp = "@" & cn & "@" DO
        EVAL map.put(cp, DoTran(CitTextUtils.ReplacePrefix(cn, Def, LibNames[lib])))
      END
    END
  END MapCells;

PROCEDURE OldDoSim() =
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
  END OldDoSim;

VAR sims := NEW(SimulationSeq.T).init();
  
PROCEDURE DoSim() =
  BEGIN
    FOR i := 0 TO MAX(0, batches - 1) DO BuildOneSim(i)  END;
    FOR i := 0 TO MAX(0, batches - 1) DO LaunchOneSim(i) END;
    FOR i := 0 TO MAX(0, batches - 1) DO LandOneSim(i)   END;
    Process.SetWorkingDirectory(runDir)
  END DoSim;

PROCEDURE BuildOneSim(i : CARDINAL) =
  CONST
    SimPath = "/p/hdk/cad/hspice/U-2023.03-SP2/hspice/bin/hspice";
  VAR
    cmd : TEXT;
  BEGIN
    Debug.Out(F("BuildOneSim(%s)", Int(i)));
    cmd    := F("%s -mt 4 -i %s", SimPath, "circuit.sp");

    IF doNetbatch THEN
      cmd := F("nbjob run %s --mode interactive %s",
                       nbopts,
                       cmd)
    END;
    
    WITH theSim = Simulation.T { cmd, Subdir(i), NIL } DO
      <*ASSERT sims.size() = i*>
      sims.addhi(theSim)
    END
  END BuildOneSim;

PROCEDURE LaunchOneSim(i : CARDINAL) =
  VAR
    theSim := sims.get(i);
    stdout, stderr := ProcUtils.WriteHere(Stdio.stderr);
  BEGIN
    Debug.Out(F("LaunchOneSim(%s)", Int(i)));
    Process.SetWorkingDirectory(Subdir(i));

    WITH cmdWr = FileWr.Open("sim.cmd") DO
      Wr.PutText(cmdWr,theSim.cmd);
      Wr.PutChar(cmdWr, '\n');
      Wr.Close(cmdWr)
    END;
    
    theSim.cm := ProcUtils.RunText(theSim.cmd,
                                   stdout := stdout,
                                   stderr := stderr,
                                   stdin  := NIL);
    sims.put(i, theSim)
  END LaunchOneSim;

PROCEDURE LandOneSim(i : CARDINAL) =
  VAR
    theSim := sims.get(i);
  BEGIN
    Debug.Out(F("LandOneSim(%s)", Int(i)));
    TRY
      theSim.cm.wait()
    EXCEPT
      ProcUtils.ErrorExit(err) =>
      Debug.Error(F("Couldn't run simulator (%s) : %s",
                    theSim.cmd,
                    ProcUtils.FormatError(err)))
    END
  END LandOneSim;


VAR conversions := NEW(RefSeq.T).init();
  
PROCEDURE LaunchConvert1(fsdbRoot : Pathname.T) =
  CONST
    CtPath  = "spice/ct/AMD64_LINUX/ct";
    SzPath  = "spice/spicecompress/spicestream/AMD64_LINUX/spicestream";
    NrPath  = "spice/fsdb/src/nanosimrd";
  VAR
    Ct     := m3utils & "/" & CtPath;
    Sz     := m3utils & "/" & SzPath;
    Nr     := m3utils & "/" & NrPath;
    cmd    := FN("%s -runtimelimit %s -fsdb %s -threads 4 -R 5e-12 -compress %s -format CompressedV1 -translate -wd %s.ctwork %s.fsdb %s",
                 TA { Ct,
                      LR(ctRuntimeLimit),
                      Nr,
                      Sz,
                      fsdbRoot,
                      fsdbRoot,
                      fsdbRoot });
    
  BEGIN
    IF convNetbatch THEN
      cmd := F("nbjob run %s --mode interactive %s",
                       nbopts,
                       cmd)
    END;
    
    WITH
          stdout = ProcUtils.WriteHere(Stdio.stderr),
          stderr = ProcUtils.WriteHere(Stdio.stderr),
          cm     = ProcUtils.RunText(cmd,
                                        stdout := stdout,
                                        stderr := stderr,
                                        stdin  := NIL)
     DO
      IF doNetbatch THEN
        conversions.addhi(cm)
      ELSE
        TRY
          cm.wait()
        EXCEPT
          ProcUtils.ErrorExit(err) =>
          Debug.Error(F("Couldn't run convert1 (%s) : %s",
                        cmd,
                        ProcUtils.FormatError(err)))
        END
      END
    END
  END LaunchConvert1;
  
PROCEDURE ConvOne(dir : Pathname.T) =
  VAR
    iter := FS.Iterate(dir);
    fn   : Pathname.T;
  BEGIN
    WHILE iter.next(fn) DO
      fn := CitTextUtils.CheckSuffix(dir & "/" & fn, ".fsdb");
      IF fn # NIL THEN
        LaunchConvert1(fn)
      END
    END
  END ConvOne;

PROCEDURE DoConv() =
  BEGIN
    DoEverywhere(ConvOne);
    WHILE conversions.size() # 0 DO
      WITH cm = NARROW(conversions.remlo(), ProcUtils.Completion) DO
        TRY
          cm.wait()
        EXCEPT
          ProcUtils.ErrorExit(err) =>
          Debug.Error(F("Couldn't run convert1 (UNKNOWN) : %s",
                        ProcUtils.FormatError(err)))
        END
      END
    END
  END DoConv;
  
TYPE Action = PROCEDURE(dir : Pathname.T);
     
PROCEDURE DoEverywhere(action : Action) =
  BEGIN
    IF batches = 0 THEN
      action(runDir)
    ELSE
      FOR i := 0 TO batches - 1 DO
        action(Subdir(i))
      END;
      action(runDir)
    END
  END DoEverywhere;
  
PROCEDURE DoClean() = BEGIN DoEverywhere(CleanOne) END DoClean;

PROCEDURE CleanOne(dir : Pathname.T) =
  VAR
    iter := FS.Iterate(dir);
    fn   : Pathname.T;
  BEGIN
    DeleteMatching(dir, "\\.ic0$");
    DeleteMatching(dir, "\\.mc0$");
    DeleteMatching(dir, "\\.fsdb$");
    DeleteMatching(dir, "\\.ava\\.");
    WHILE iter.next(fn) DO
      IF CitTextUtils.HaveSuffix(fn, ".ctwork") THEN
        DeleteRecursively(dir, fn)
      END
    END;
    FOR f := FIRST(Files) TO LAST(Files) DO
      CompressFile(dir & "/" & Files[f])
    END;
    CompressFile(dir & "/" & "the_scaled_dut.sp");
  END CleanOne;
  
TYPE
  Phase = { Pre, Sim, Conv, Clean, Post };
  Proc = PROCEDURE();
  Lib   = { I0s, I0m };

CONST
  PhaseNames = ARRAY Phase OF TEXT       { "pre", "sim", "conv", "clean", "post" };
  PhaseProc  = ARRAY Phase OF Proc       { DoPre, DoSim, DoConv, DoClean, DoPost };
  LibNames   = ARRAY Lib   OF TEXT       { "i0s", "i0m"  };
  LibPaths   = ARRAY Lib   OF Pathname.T { "lib783_i0s_160h_50pp", "lib783_i0m_180h_50pp" };

  DefStep    = 20.0d-9;
  DefSweeps  = 4;
  DefA       = 16_5befa033;
  DefB       = 16_44110510;
   
VAR
  pp                             := NEW(ParseParams.T).init(Stdio.stderr);
  vdd, temp                      := FIRST(LONGREAL);
  traceRt       : TEXT;
  step          : LONGREAL       := DefStep;
  phases                         := SET OF Phase { FIRST(Phase) .. LAST(Phase) };
  measureFn     : Pathname.T     := "measure.dat";
  tag           : TEXT           := "";
  aInput        : Word.T         := DefA;
  bInput        : Word.T         := DefB;
  lib           : Lib;
  sweeps        : CARDINAL       := DefSweeps;
  m3utils                        := Env.Get("M3UTILS");
  tran          : Tran           := Tran.Ulvt;
  cscale, rscale                 := 1.0d0;
  deln, delp                     := 0.0d0;
  modLeaves                      := TRUE;
  stages        : CARDINAL       := 0;

  externalSweep : CARDINAL       := 0; (* how big the steps are for external
                                          sweeps *)

  batches       : CARDINAL       := 0; (* 0 means sweep internally *)
  runDir                         := FS.GetAbsolutePathname(".");

  ctRuntimeLimit                 := 24.0d0 * 3600.0d0;
  
  doNetbatch    : BOOLEAN;
  convNetbatch  : BOOLEAN;
  
  nbopts        : TEXT;

  doNominal : BOOLEAN;
  doDeterministic : BOOLEAN;
  
BEGIN
  IF m3utils = NIL THEN
    Debug.Error("?must set M3UTILS")
  END;
  
  TRY
    doNetbatch   := pp.keywordPresent("-netbatch") OR pp.keywordPresent("-nb");
    convNetbatch := pp.keywordPresent("-convnetbatch") OR pp.keywordPresent("-cnb");

    doNominal := pp.keywordPresent("-nominal");
    doDeterministic := pp.keywordPresent("-deterministic");

    IF doNetbatch THEN
      IF NbOpts = NIL AND NbPool = NIL THEN
        Debug.Error("?when using netbatch, must set NBPOOL or NBRUNOPTIONS")
      END;
      IF NbOpts # NIL THEN
        nbopts := NbOpts
      ELSE
        nbopts := F("--target %s --class 4C --class SLES12", NbPool);
      END
    END;

    IF pp.keywordPresent("-ctruntimelimit") THEN
      ctRuntimeLimit := pp.getNextLongReal()
    END;
    
    IF pp.keywordPresent("-t") THEN
      traceRt := pp.getNext()
    END;
    IF pp.keywordPresent("-vdd") THEN
      vdd := pp.getNextLongReal();
      <*ASSERT vdd > 0.0d0*>
    END; 
    IF pp.keywordPresent("-temp") THEN
      temp := pp.getNextLongReal();
      <*ASSERT temp > -274.0d0*>
    END; 
    IF pp.keywordPresent("-tag") THEN
      tag := pp.getNext()
    END; 

    IF pp.keywordPresent("-debugfile") OR pp.keywordPresent("-dbgf") THEN
      WITH fn = pp.getNext() DO
        TRY
          WITH wr = FileWr.Open(fn) DO
            Debug.AddStream(wr)
          END
        EXCEPT
          OSError.E(x) => Debug.Error(F("Can't add debug stream to file \"%s\" : OSError.E : %s", fn, AL.Format(x)))
        END
      END
    END;
    
    IF pp.keywordPresent("-sweeps") THEN
      sweeps := pp.getNextInt();
    END;
    IF doNominal THEN
      <*ASSERT sweeps=1*>
    ELSE
      IF sweeps < 2 THEN
        Debug.Warning("Statistical work requested with few sweeps : " &
          Int(sweeps))
      END
    END;
    
    IF pp.keywordPresent("-externalsweep") THEN
      externalSweep := pp.getNextInt()
    END;

    IF externalSweep # 0 THEN
      batches := (sweeps - 1) DIV externalSweep + 1
    END;
    
    IF pp.keywordPresent("-step") THEN
      step := pp.getNextLongReal()
    END;
    IF pp.keywordPresent("-stages") THEN
      stages := pp.getNextInt()
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
    IF pp.keywordPresent("-thresh") THEN
      tran := VAL(Lookup(pp.getNext(), TranNames),
                                           Tran)
    END;

    IF pp.keywordPresent("-modleaves") THEN
      modLeaves := Scan.Bool(pp.getNext())
    END;

    IF pp.keywordPresent("-cscale") THEN
      cscale := pp.getNextLongReal();
      rscale := Math.pow(cscale, -1.8d0);
    END;

    IF pp.keywordPresent("-deln") THEN
      deln := pp.getNextLongReal()
    END;

    IF pp.keywordPresent("-delp") THEN
      delp := deln + pp.getNextLongReal()
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


END LowTemp.
