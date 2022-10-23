MODULE Main;

(*
  Technology comparison across

  P1276P4
  N5
  N3 (vaporware process)
  N3E

  including all provided transistor thresholds

  Author : mika.nystroem@intel.com
  October, 2022

*)

IMPORT ParseParams;
IMPORT Text;
IMPORT Debug;
IMPORT Stdio;
FROM Fmt IMPORT F, Int, FN; IMPORT Fmt;
IMPORT OSError;
IMPORT AL;
IMPORT Process;
IMPORT Pathname;
IMPORT TextTextTbl;
IMPORT TextSeq;
IMPORT Rd, Wr;
IMPORT FileRd, FileWr;
IMPORT CitTextUtils;
IMPORT Thread;
IMPORT ProcUtils;
IMPORT TextWr;
IMPORT Trace;
IMPORT LongRealSeq;
IMPORT Math;
IMPORT FS;
IMPORT Watchdog;
IMPORT Scan;
IMPORT Lex;
IMPORT Time;
IMPORT RegEx;
IMPORT Usignal;
IMPORT FloatMode;
IMPORT TechProcess;
IMPORT N5TechProcess, N3TechProcess, N3ETechProcess;
IMPORT P1276p4TechProcess, P1278p3TechProcess;

FROM TechConfig IMPORT Tech, Corp, Tran, Mode, Phaz, Simu, Corn, Gate;

FROM TechConfig IMPORT TranNames, ModeNames, PhazNames, SimuNames, CornNames, GateNames, TechNames, TechCorp;

FROM TechConfig IMPORT Gate1, SupportedFanouts, CorpNames;




<*FATAL Thread.Alerted*>

CONST TE = Text.Equal;
      LR = Fmt.LongReal;

(* to add a new Tech to the system:

   Add to Tech, then update
   TechNames
   TranSufxs
   TechTranSufxs
   TechTranSizes
   TechHspiceModels
   TechHspiceModelRoots
   TechCornNames
   MapTech
   TechParaCellName
   TechPlugText
   TechStdCellPaths

   *** And if needed: ***
   Tran (if you have a new transistor type) 
     TranNames
     TranSufxs for existing processes with NIL in the new slot
     ApproxThresh
*)

VAR
  Verbose := Debug.DebugThis("techc");
  
  
CONST DefProcDeadline = 30.0d0 * 60.0d0;
      (* give it 30 minutes for each subprocess step (circuit sim and aspice
         data conversion) *)
      ParasiticDeadlineMultiplier = 2.0d0;

      FirstProgressDelay = 2.0d0 * 60.0d0;
      (* when to check for progress / two minutes *)
      
      ProgressDelayMultiplier = 1.414d0;
      (* how much to back off each progress measurement *)
      
VAR ProcDeadline := DefProcDeadline;

CONST    
  StdPlugText = "vcc vssx";


    
CONST
  Techs = ARRAY Tech OF TechProcess.T { N5TechProcess.P,
                                        P1276p4TechProcess.P,
                                        N3TechProcess.P,
                                        N3ETechProcess.P,
                                        P1278p3TechProcess.P
                                        };
    
  (************************************************************)

  ApproxThresh = ARRAY Tran OF LONGREAL { 0.150d0,
                                          0.250d0,
                                          0.300d0,
                                          0.350d0,
                                          0.400d0,
                                          0.450d0,
                                          0.500d0 };

  ApproxCornThreshShift = ARRAY Corn OF LONGREAL {  0.000d0,
                                                    0.025d0,
                                                   -0.005d0,
                                                    0.005d0,
                                                    0.005d0 };

  AbsZero = -273.15d0; (* absolute zero in degrees Celsius *)
  
CONST
  XaOptions =
    ".OPTION CMIFLAG=1 CMIUSRFLAG=3 PDMI=1\n"  &
    ".OPTION POST=fsdb PROBE=1\n" &
    ".OPTION XA_CMD=\"set_sim_level -level 6\"\n" &
    ".OPTION XA_CMD=\"set_wildcard_rule -match* one\"\n" &
    ".OPTION XA_CMD=\"set_message_option -limit 100\"\n" &
    ".OPTION XA_CMD=\"enable_print_statement 1\"\n" &
    ".OPTION XA_CMD=\"set_sim_case -case sensitive\"";

  HspiceOptions = "";

  SimOptions = ARRAY Simu OF TEXT { XaOptions, HspiceOptions };

TYPE
  RunPhase = PROCEDURE(READONLY c : Config);

CONST
  Phases = ARRAY Phaz OF RunPhase { DoSetup,
                                    DoSimulate,
                                    DoConvertPhaz,
                                    DoClean,
                                    DoMeasurePhaz
  };
  
PROCEDURE Lookup(str : TEXT; READONLY a : ARRAY OF TEXT) : CARDINAL =
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      IF TE(str, a[i]) THEN RETURN i END
    END;
    VAR
      str := F("could not find %s among alternatives :");
    BEGIN
      FOR i := FIRST(a) TO LAST(a) DO
        str := str & F( " \"%s\"" , a[i])
      END;
      Debug.Error(str)
    END;
    <*ASSERT FALSE*>
  END Lookup;

CONST CornDelay = ARRAY Corn OF LONGREAL { 1.0d0, 3.0d0, 0.8d0, 2.0d0, 2.0d0 };
      
PROCEDURE MapCommon(READONLY c : Config;  map : TextTextTbl.T)=
  VAR
    iter : TextTextTbl.Iterator;
    k, v : TEXT;
    tech := Techs[c.tech];
  BEGIN
    EVAL map.put("@HSPICE_MODEL_ROOT@", c.hspiceModelRoot);
    EVAL map.put("@HSPICE_MODEL@", c.hspiceModel);
    EVAL map.put("@TEMP@", LR(c.temp));
    EVAL map.put("@VOLT@", LR(c.volt));
    EVAL map.put("@FANOUT@", Int(c.fanout));

    (* gate terminals *)
    CASE c.gate OF
      Gate.Buf =>
      EVAL map.put("@T0A@", "in");
      EVAL map.put("@T0B@", "");
      EVAL map.put("@T0C@", "");

      EVAL map.put("@T1A@", "xi");
      EVAL map.put("@T1B@", "");
      EVAL map.put("@T1C@", "");
    |
      Gate.Xor =>
      EVAL map.put("@T0A@", "in");
      EVAL map.put("@T0B@", "vcc");
      EVAL map.put("@T0C@", "");

      EVAL map.put("@T1A@", "vcc");
      EVAL map.put("@T1B@", "xi");
      EVAL map.put("@T1C@", "");
    |
      Gate.Aoi =>
      (* Intel terminal order is a b c  where b & c are the symmetric inputs
         TSMC terminal order is A1 A2 B *)
      CASE TechCorp[c.tech] OF
        Corp.Intc =>
        EVAL map.put("@T0A@", "vssx");
        EVAL map.put("@T0B@", "in");
        EVAL map.put("@T0C@", "in");
        
        EVAL map.put("@T1A@", "vcc");
        EVAL map.put("@T1B@", "xi");
        EVAL map.put("@T1C@", "xi");
      |
        Corp.Tsmc =>
        EVAL map.put("@T0A@", "in");
        EVAL map.put("@T0B@", "in");
        EVAL map.put("@T0C@", "vssx");
        
        EVAL map.put("@T1A@", "xi");
        EVAL map.put("@T1B@", "xi");
        EVAL map.put("@T1C@", "vcc");
      END        
    |
      Gate.Oai =>
      <*ASSERT FALSE*>
    END;
    
    (* parasitic or not *)
    IF c.para THEN
      WITH gate1     = Gate1[c.gate],
           cellname0 = tech.cellNames[c.gate][c.tran],
           cellname1 = tech.cellNames[gate1 ][c.tran] DO
        <*ASSERT cellname0 # NIL*>
        <*ASSERT cellname1 # NIL*>
        EVAL map.put("@CELLNAME0@", cellname0);
        EVAL map.put("@CELLNAME1@", cellname1)
      END;
      EVAL map.put("@PLUGTEXT@", tech.plugText);
      WITH p0 =        tech.cellPaths[c.gate][c.tran],
           g1 = Gate1[c.gate],
           p1 =        tech.cellPaths[g1    ][c.tran] DO
        IF TE(p0, p1) THEN
          EVAL map.put("@INCLUDELIB@", F(".include \"%s\"\n",
                                         p0));
        ELSE
          EVAL map.put("@INCLUDELIB@", F(".include \"%s\"\n.include \"%s\"\n",
                                         p0, p1));
        END
      END
        
    ELSE
      (* not parasitic *)
      WITH gate1     = Gate1[c.gate],
           cellname0 = GateNames[c.gate] & "_gate",
           cellname1 = GateNames[gate1]  & "_gate" DO
        <*ASSERT cellname0 # NIL*>
        <*ASSERT cellname1 # NIL*>
        EVAL map.put("@CELLNAME0@", cellname0);
        EVAL map.put("@CELLNAME1@", cellname1)
      END;
      EVAL map.put("@CELLNAME0@", GateNames[c.gate] & "_gate");
      EVAL map.put("@CELLNAME1@", GateNames[c.gate] & "_gate");
      EVAL map.put("@PLUGTEXT@", StdPlugText);
      EVAL map.put("@INCLUDELIB@", "");
      EVAL map.put("@OPTVCC@", "vcc"); (*  vcc input for XOR *)
    END;
    
    EVAL map.put("@NANOSECONDS@", Int(CEILING(c.nanoseconds)));
    EVAL map.put("@TIMESTEP@", Int(ROUND(c.timestep / 1.0d-12)) & "ps");
    EVAL map.put("@OPTIONS@", SimOptions[c.simu]);
    EVAL map.put("@CORNER@", tech.cornNames[c.corn]);

    CASE c.mode OF
      Mode.Dyn =>
      EVAL map.put("@RESET_SOURCE@",
                   "Vres _RESET 0 DC=0 PWL 0 0 10ns 0 10.1ns vtrue")
    |
      Mode.Leak =>
      EVAL map.put("@RESET_SOURCE@", "Vres _RESET 0 DC=0")
    END;

    WITH sufx = tech.tranSufxs[c.tran] DO
      IF sufx = NIL THEN
        Debug.Error(F("No mapping for %s in %s",
                      TranNames[c.tran],
                      TechNames[c.tech]))
      END;
      EVAL map.put("@TRANSUFX@", sufx)
    END;

    EVAL map.put("@TRANSIZE@", tech.tranSize);

    iter := extraMap.iterate();
    WHILE iter.next(k, v) DO
      WITH key = F("@%s@", k),
           hadIt = map.put(key, v) DO
        IF hadIt THEN
          Debug.Error("Duplicate mapping for key " & key)
        END
      END
    END;

    iter := overrideMap.iterate();
    WHILE iter.next(k, v) DO
      WITH key   = F("@%s@", k),
           <*NOWARN*>hadIt = map.put(key, v) DO
        (* skip *)
      END
    END
  END MapCommon;

PROCEDURE LoadTemplate(path : Pathname.T) : TextSeq.T
  RAISES { OSError.E, Rd.Failure } =
  VAR
    rd := FileRd.Open(path);
    res := NEW(TextSeq.T).init();
  BEGIN
    TRY
      LOOP
        WITH line = Rd.GetLine(rd) DO
          res.addhi(line)
        END
      END
    EXCEPT
      Rd.EndOfFile => (* skip *)
    END;
    Rd.Close(rd);
    RETURN res
  END LoadTemplate;

PROCEDURE WriteTemplate(template : TextSeq.T; path : Pathname.T)
  RAISES { OSError.E, Wr.Failure } =
  VAR
    wr := FileWr.Open(path);
  BEGIN
    FOR i := 0 TO template.size() - 1 DO
      Wr.PutText(wr, template.get(i));
      Wr.PutChar(wr, '\n')
    END;
    Wr.Close(wr)
  END WriteTemplate;

PROCEDURE ModifyTemplate(template : TextSeq.T; map : TextTextTbl.T) =
  VAR
    k, v, line : TEXT;
    iter := map.iterate();
  BEGIN
    WHILE iter.next(k, v) DO
      IF Verbose THEN
        Debug.Out(F("k %s -> v %s", k, Debug.UnNil(v)))
      END
    END;
    
    FOR i := 0 TO template.size() - 1 DO
      line := template.get(i);
      iter := map.iterate();
      WHILE iter.next(k, v) DO
        IF v = NIL THEN
          Debug.Error("NIL mapping for " & k)
        END;
        line := CitTextUtils.Replace(line, k, v)
      END;
      template.put(i, line)
    END
  END ModifyTemplate;

CONST DefSimRoot = "circuit";

PROCEDURE DoCommonSetup(VAR c : Config) =
  CONST
    DefaultTimeStep = 1.0d-12;
    MaxTimeSteps    = 50000.0d0;

  BEGIN
    WITH deltaV = c.volt - (ApproxThresh[c.tran] + ApproxCornThreshShift[c.corn]),
         stepsV = deltaV / 0.035d0,  (* kT/q *)
         threshDelayFactor = Math.exp(-stepsV),
         
         kelvinTemp      = c.temp - AbsZero,
         baseTemp        = 120.0d0 - AbsZero,
         tempDelayFactor = Math.pow(kelvinTemp / baseTemp, -1.5d0),
         cornDelayFactor = CornDelay[c.corn],
         delayFactor     = (1.0d0 + threshDelayFactor) * tempDelayFactor * cornDelayFactor,
         nanoseconds     = 10.0d0 +
                           ParaNanoFactor[c.para] * 10.0d0 * (delayFactor + 1.5d0),
         timestep        = MAX(DefaultTimeStep,
                               nanoseconds * 1.0d-9 / MaxTimeSteps)
     DO
      Debug.Out(F("tempDelayFactor %s, thresDelayFactor %s, delayFactor %s, nanoseconds %s",
                  LR(tempDelayFactor),
                  LR(threshDelayFactor),
                  LR(delayFactor),
                  LR(nanoseconds)));
      
      c.nanoseconds := nanoseconds;
      c.timestep    := timestep
    END;
  END DoCommonSetup;

CONST ParaNanoFactor = ARRAY BOOLEAN OF LONGREAL { 1.0d0, 2.0d0 };
      
PROCEDURE DoSetup(READONLY c : Config) =
  VAR
    SimFile := c.simRoot & ".sp";
    map     := NEW(TextTextTbl.Default).init();
    template : TextSeq.T;
  BEGIN
    MapCommon(c, map);

    TRY
      template := LoadTemplate(c.templatePath);
    EXCEPT
      OSError.E(e) => Debug.Error(F("Couldn't open template file \"%s\" : OSError.E : %s",
                                    c.templatePath, AL.Format(e)))
    |
      Rd.Failure(e) =>
      Debug.Error(F("Couldn't read template file \"%s\" : Rd.Failure : %s",
                                    c.templatePath, AL.Format(e)))
    END;

    ModifyTemplate(template, map);

    TRY
      WriteTemplate(template, SimFile)
    EXCEPT
      OSError.E(e) => Debug.Error(F("Couldn't write simulation file \"%s\" : OSError.E : %s",
                                    SimFile, AL.Format(e)))
    |
      Wr.Failure(e) =>
      Debug.Error(F("Couldn't write simulation file \"%s\" : Wr.Failure : %s",
                    SimFile, AL.Format(e)))
    END
  END DoSetup;

TYPE
  MyCb = Watchdog.Callback OBJECT
    cmd : TEXT;
    wr  : TextWr.T;
  OVERRIDES
    do := MyCbDo;
  END;

  MyKillCb = MyCb OBJECT
    simRoot : Pathname.T;
    myKill  : REF BOOLEAN; (* set to TRUE if killing *)
  OVERRIDES
    do := MyCbDoKillSim;
  END;

PROCEDURE MyCbDo(cb : MyCb) =
  BEGIN
    Debug.Out(F("\n!!! WOOF WOOF !!!\nCommand \"%s\" with output\n====>\n%s\n<====\n\nWatchdog expired!  Exiting!",
                cb.cmd,
                TextWr.ToText(cb.wr)));
    Process.Exit(1)
  END MyCbDo;

PROCEDURE MyCbDoKillSim(cb : MyKillCb) =
  BEGIN
    Debug.Out(F("\n!!! WOOF WOOF !!!\nCommand \"%s\" --- Watchdog expired!  Attempting to kill simulator!",
                cb.cmd));

    WITH logFn = cb.simRoot & ".log" DO
      TRY
        cb.myKill^ := TRUE;
        KillFromLogFile(logFn);

        Debug.Out(F("Command output was\n====>\n%s\n<====\n\nWatchdog expired!",
                TextWr.ToText(cb.wr)));

        
      EXCEPT
        OSError.E(e) => Debug.Warning(F("Couldn't open log file \"%s\" : OSError.E : %s",
                                      logFn, AL.Format(e)))
      |
        Rd.Failure(e) =>
        Debug.Warning(F("Couldn't read log file \"%s\" : Rd.Failure : %s",
                      logFn, AL.Format(e)))
      END
    END;
    
    Debug.Out(F("\n!!! WOOF WOOF !!!\nCommand \"%s\" with output\n====>\n%s\n<====\n\nWatchdog expired!  Exiting!",
                cb.cmd,
                TextWr.ToText(cb.wr)))
  END MyCbDoKillSim;
  

TYPE
  SimWatcher = Thread.Closure OBJECT
    cm      : ProcUtils.Completion;
    simRoot : Pathname.T;
    myKill  : REF BOOLEAN; (* set to TRUE if killing *)
  OVERRIDES
    apply := SwApply;
  END;

PROCEDURE Kill(id : CARDINAL) =
  CONST
    DoDebug = TRUE;
  BEGIN
    (* INT needs to be the first signal sent *)
    IF DoDebug THEN
      Debug.Out(F("Abort killing sub: INT (%s,%s)",
                  Int(id), Int(Usignal.SIGINT)))
    END;
    EVAL Usignal.kill(id, Usignal.SIGINT); (* INTerrupt *)

    Thread.Pause(5.0d0); (* give it time to write out the log file *)

    IF DoDebug THEN
      Debug.Out(F("Abort killing sub: TERM (%s,%s)",
                  Int(id), Int(Usignal.SIGTERM)))
    END;
    EVAL Usignal.kill(id, Usignal.SIGTERM); (* TERMinate *)
    
    Thread.Pause(1.0d0);

    IF DoDebug THEN
      Debug.Out(F("Abort killing sub: KILL (%s,%s)",
                  Int(id), Int(Usignal.SIGKILL)))
    END;
    EVAL Usignal.kill(id, Usignal.SIGKILL); (* KILL *)
  END Kill;

CONST
  ProgressRoot = "progress";
  
PROCEDURE SwApply(sw : SimWatcher) : REFANY =
  CONST
    FirstDelay   = FirstProgressDelay;
  VAR
    d := FirstDelay;
    now : Time.T;
  BEGIN
    LOOP
      now := Time.Now();
      WITH deadLine = now + d DO
        REPEAT
          Thread.Pause(1.0d0);
          now := Time.Now()
        UNTIL now >= deadLine
      END;
      
      Debug.Out(F("SimWatcher waking up at %s, d=%s", LR(Time.Now()), LR(d)));

      (* trial conversion *)
      IF DoConvert(ProgressRoot, exitOnError := FALSE) AND
         DoMeasure(ProgressRoot, ProgressRoot & ".dat", sw.simRoot) THEN
        (* succeeded, we have the output! *)
        
        Debug.Out("Progress measurement succeeded, will try to kill simulator!");


        WITH logFn = sw.simRoot & ".log" DO

          TRY
            sw.myKill^ := TRUE;
            KillFromLogFile(logFn)

          EXCEPT
            OSError.E(e) => Debug.Error(F("Couldn't open log file \"%s\" : OSError.E : %s",
                                          logFn, AL.Format(e)))
          |
            Rd.Failure(e) =>
            Debug.Error(F("Couldn't read log file \"%s\" : Rd.Failure : %s",
                          logFn, AL.Format(e)))
          END;
        END;
        RETURN NIL
      END(*IF*);
      
      d := d * ProgressDelayMultiplier
    END
  END SwApply;

PROCEDURE KillFromLogFile(logFn : Pathname.T)
  RAISES { Rd.Failure, OSError.E } =
  
  <*FATAL RegEx.Error*>
  <*FATAL FloatMode.Trap, Lex.Error*>
  VAR
    numPat := RegEx.Compile("^[1-9][0-9]*$");
    rd := FileRd.Open(logFn);
  BEGIN
    TRY
      LOOP
        WITH line  = Rd.GetLine(rd),
             match = CitTextUtils.HaveSub(line, " pid ") DO

          Debug.Out(F("logfileline %s match %s", line, Fmt.Bool(match)));
          
          IF match THEN
            VAR
              sh := CitTextUtils.Shatter(line,
                                         delims := " |\t",
                                         endDelims := "",
                                         skipNulls := TRUE);
              p := sh;
            BEGIN
              WHILE p # NIL DO
                IF RegEx.Execute(numPat, p.head) # -1 THEN
                  WITH pid = Scan.Int(p.head) DO
                    Debug.Out(F("Regex match %s, attempting to kill",
                              Int(pid)));
                    Kill(pid)
                  END
                END;
                p := p.tail
              END
            END
          END
        END
      END
    EXCEPT
      Rd.EndOfFile => (* skip *)
    END
  END KillFromLogFile;

PROCEDURE DoSimulate(READONLY c : Config) =
  <*FATAL OSError.E*> (* this pertains to the TextWr *)
  CONST
    Affirmation = "y\ny\ny\ny\ny\n";
  VAR
    wr := NEW(TextWr.T).init();
    stdout, stderr := ProcUtils.WriteHere(wr);

    cmd := F("%s/xa %s.sp -o %s", c.xaPath, c.simRoot, c.simRoot);
    myKill := NEW(REF BOOLEAN);
  BEGIN

    myKill^ := FALSE;
    
    (*Wr.Close(wrIn);*)
    CASE c.simu OF
      Simu.Xa =>
      Debug.Out("DoSimulate: " & cmd);
      WITH wd = NEW(Watchdog.T).init(ProcDeadline),
           cm = ProcUtils.RunText(cmd,
                                  stdout := stdout,
                                  stderr := stderr,
                                  stdin  := ProcUtils.ReadThis(Affirmation)),

           (* we need to feed in the affirmation, because when we hit xa with
              kill -INT, it stops and asks the y/n question whether it should
              exit... *)
           
           cb = NEW(MyKillCb, myKill := myKill, cmd := cmd, wr := wr, simRoot := c.simRoot) DO
        
        wd.setExpireAction(cb);
        EVAL Thread.Fork(NEW(SimWatcher, myKill := myKill, cm := cm, simRoot := c.simRoot));
        TRY
          cm.wait()
        EXCEPT
          ProcUtils.ErrorExit(err) =>
          WITH msg = F("command \"%s\" with output\n====>\n%s\n<====\n\nraised ErrorExit : %s",
                        cmd,
                        TextWr.ToText(wr),
                        ProcUtils.FormatError(err)) DO
            IF myKill^ THEN
              Debug.Warning(msg)
            ELSE
              Debug.Error(msg)
            END
          END
        END;
        wd.kill()
      END
    |
      Simu.Hspice => <*ASSERT FALSE*>
    END;
    Debug.Out("DoSimulate output :\n" & TextWr.ToText(wr))
  END DoSimulate;

PROCEDURE DoConvertPhaz(READONLY c : Config) =
  BEGIN
    EVAL DoConvert(c.simRoot, exitOnError := TRUE)
  END DoConvertPhaz;

(* semaphore for Convert phase *)
VAR
  convertMu := NEW(MUTEX);
  convertC  := NEW(Thread.Condition);
  convertS  : [0..1] := 0;

(* Dijkstra's P() and V() *)
PROCEDURE ConvertP() =
  BEGIN
    LOCK convertMu DO
      WHILE convertS = 1 DO
        Thread.Wait(convertMu, convertC)
      END;
      INC(convertS)
    END
  END ConvertP;

PROCEDURE ConvertV() =
  BEGIN
    LOCK convertMu DO
      DEC(convertS);
      Thread.Signal(convertC)
    END
  END ConvertV;
  
PROCEDURE DoConvert(traceRoot : Pathname.T; exitOnError : BOOLEAN) : BOOLEAN =
  <*FATAL OSError.E*> (* this pertains to the TextWr *)
  VAR
    wr := NEW(TextWr.T).init();
    stdout, stderr := ProcUtils.WriteHere(wr);

    cmd := F("/nfs/site/disks/zsc3_fon_fe_0001/mnystroe/m3utils/spice/ct/AMD64_LINUX/ct -fsdb /nfs/site/disks/zsc3_fon_fe_0001/mnystroe/m3utils/spice/fsdb/src/nanosimrd -threads 4 -wthreads 1 -R %s %s.fsdb %s",
             LR(MAX(c.timestep, 50.0d-12)), c.simRoot, traceRoot);
    res : BOOLEAN := FALSE;
  BEGIN 
    (*Wr.Close(wrIn);*)
    ConvertP();
    
    TRY
      CASE c.simu OF
        Simu.Xa =>
        Debug.Out("DoConvert: " & cmd);
        WITH wd = NEW(Watchdog.T).init(ProcDeadline),
             c  = ProcUtils.RunText(cmd,
                                    stdout := stdout,
                                    stderr := stderr,
                                    stdin  := NIL),
             cb = NEW(MyCb, cmd := cmd, wr := wr) DO
               wd.setExpireAction(cb);
               TRY
                 c.wait();
                 res := TRUE
               EXCEPT
                 ProcUtils.ErrorExit(err) =>
                 WITH msg = F("command \"%s\" with output\n====>\n%s\n<====\n\nraised ErrorExit : %s",
                              cmd,
                              TextWr.ToText(wr),
                              ProcUtils.FormatError(err)) DO
                   IF exitOnError THEN
                     Debug.Error(msg)
                   ELSE
                     Debug.Warning(msg);
                     res := FALSE
                   END
                 END
               END;
               wd.kill()
             END
      |
        Simu.Hspice => <*ASSERT FALSE*>
      END;
      Debug.Out("DoConvert output :\n" & TextWr.ToText(wr));
    FINALLY
      ConvertV()
    END;
    RETURN res
  END DoConvert;

PROCEDURE DoClean(READONLY c : Config) =
  BEGIN
    TRY FS.DeleteFile(F("%s.fsdb", c.simRoot)) EXCEPT ELSE END;

    TRY
      CONST
        CtWorkDir = "ct.work";
      VAR
        iter := FS.Iterate(CtWorkDir);
        fn : Pathname.T;
      BEGIN
        WHILE iter.next(fn) DO
          WITH ffn = CtWorkDir & "/" & fn DO
            IF Verbose THEN
              Debug.Out("Attempting to delete "& ffn)
            END;
            TRY FS.DeleteFile(ffn) EXCEPT ELSE END
          END
        END
      END;
      IF Verbose THEN
        Debug.Out("Attempting to delete "& "ct.work")
      END;
      FS.DeleteDirectory("ct.work")
    EXCEPT ELSE END
  END DoClean;

PROCEDURE DoMeasurePhaz(READONLY c : Config) =
  BEGIN
    IF NOT (DoMeasure(c.simRoot, "measure.dat", c.workDir, FALSE) OR
            DoMeasure(ProgressRoot, "measure.dat", c.workDir))
     THEN Debug.Error("Measure phase failed : no measurement available")
    END
  END DoMeasurePhaz;
  
PROCEDURE DoMeasure(traceRoot, outName, workDir : Pathname.T;
                    exitOnError := TRUE) : BOOLEAN =
  (* returns TRUE iff we measure a cycle time *)
  VAR
    trace : Trace.T;
    nSteps : CARDINAL;
    timeData, nodeData : REF ARRAY OF LONGREAL;
    fail := FALSE;
    
  PROCEDURE Fail(msg : TEXT) =
    BEGIN
      fail := TRUE;
      IF exitOnError THEN
        Debug.Error(msg)
      ELSE
        Debug.Warning(msg)
      END
    END Fail;
    
  BEGIN
    Debug.Out(F("DoMeasure %s %s %s", traceRoot, outName, workDir));
    
    TRY
      trace := NEW(Trace.T).init(traceRoot);
    EXCEPT
      OSError.E(x) =>
      Fail("OSError.E reading trace/names file : " & AL.Format(x))
    |
      Rd.Failure(x) =>
      Fail("I/O error reading trace/names file : " & AL.Format(x))
    |
      Rd.EndOfFile =>
      Fail("Short read reading trace/names file")
    END;

    IF fail THEN RETURN FALSE END;

    nSteps := trace.getSteps();

    timeData := NEW(REF ARRAY OF LONGREAL, nSteps);
    nodeData := NEW(REF ARRAY OF LONGREAL, nSteps);

    TRY
      trace.getTimeData(timeData^);
    EXCEPT
      Rd.Failure, Rd.EndOfFile => Debug.Error("Trouble reading TIME data")
    END;
    
    Debug.Out(F("nSteps %s", Int(nSteps)));
    
    Debug.Out(F("first time %s, last time %s, step %s",
                LR(timeData[0]),
                LR(timeData[nSteps - 1]),
                LR(trace.getTimeStep())));
    
    CONST
      StartTime = 12.0d-9;
      StartTran = 1;
    VAR
      xIdx := GetIdx(trace, "x[0]");
      iIdx := GetIdx(trace, "vissx");
      yIdx := GetIdx(trace, "vissy");
      cycle, meancurrent, leakcurrent : LONGREAL;
    BEGIN
      TRY
        trace.getNodeData(xIdx, nodeData^);
      EXCEPT
        Rd.Failure, Rd.EndOfFile => Debug.Error("Trouble reading node data")
      END;
      
      cycle := CycleTime(timeData^, nodeData^,
                         c.volt / 2.0d0, StartTime, StartTran, StartTran + 1);
      
      Debug.Out("Measured cycle time " & LR(cycle));

      TRY
        trace.getNodeData(iIdx, nodeData^);
      EXCEPT
        Rd.Failure, Rd.EndOfFile => Debug.Error("Trouble reading node data")
      END;

      meancurrent := -1.0d0 / 1.0d6 *
          MeanValue(timeData^, nodeData^, StartTime);
      
      Debug.Out("Measured mean dyna current " & LR(meancurrent));

      TRY
        trace.getNodeData(yIdx, nodeData^);
      EXCEPT
        Rd.Failure, Rd.EndOfFile => Debug.Error("Trouble reading node data")
      END;

      leakcurrent := -1.0d0 / 1.0d6 *
          MeanValue(timeData^, nodeData^, StartTime);
      
      Debug.Out("Measured mean leak current " & LR(leakcurrent));

      TRY
        VAR
          wr := FileWr.Open(outName);
        BEGIN
          Wr.PutText(wr,
                     FN("%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n",
                        ARRAY OF TEXT {
                           TechNames[c.tech],
                           CornNames[c.corn],
                           TranNames[c.tran],
                           GateNames[c.gate],
                           ModeNames[c.mode],
                           SimuNames[c.simu],
                           Int(c.fanout),
                           LR(c.volt),
                           LR(c.temp),
                           LR(cycle),
                           LR(meancurrent),
                           LR(leakcurrent),
                           workDir
                           }));
          Wr.Close(wr)
        END;

        RETURN cycle < 1.0d10; (* cycle time is less than 10^10 seconds,
                                  i.e., it exists *)
        
      EXCEPT
        OSError.E(x) =>
        Debug.Error(F("Couldn't open measurement output file : OSError.E : %s",
                      AL.Format(x)))
      |
        Wr.Failure(x) =>
        Debug.Error(F("Couldn't write measurement output file : Wr.Failure : %s",
                      AL.Format(x)))
      END;
      <*ASSERT FALSE*>
    END
  END DoMeasure;

PROCEDURE CycleTime(READONLY timea, nodea : ARRAY OF LONGREAL;
                    cross                 : LONGREAL;
                    startTime             : LONGREAL;
                    startTran, endTran    : CARDINAL) : LONGREAL =
  (* looking at nodea values beyond startTime, 
     find the startTran and endTran rising transitions
     and return the average cycle time in that range *)
  VAR
    pn := nodea[FIRST(nodea)];
    pt : LONGREAL;
    seq := NEW(LongRealSeq.T).init();
  BEGIN
    FOR i := FIRST(timea) TO LAST(timea) DO
      WITH t = timea[i],
           n = nodea[i] DO
        IF t >= startTime THEN
          IF n > cross AND pn <= cross THEN
            WITH delV  = n - pn,
                 delT  = t - pt,
                 delV0 = cross - pn,
                 delT0 = delV0 / delV * delT DO
              seq.addhi(pt + delT0)
            END
          END
        END;
        pn := n;
        pt := t
      END
    END;

    IF endTran < seq.size() THEN
      WITH cnt = endTran - startTran,
           sT  = seq.get(startTran),
           eT  = seq.get(endTran),
           res = (eT - sT) / FLOAT(cnt, LONGREAL) DO
        RETURN res
      END
    ELSE
      RETURN LAST(LONGREAL)
    END
  END CycleTime;

PROCEDURE MeanValue(READONLY timea, nodea : ARRAY OF LONGREAL;
                    startTime             : LONGREAL) : LONGREAL =
  (* looking at nodea values beyond startTime, 
     return the mean value in the rest of history *)
  VAR
    sum := 0.0d0;
    cnt := 0;
  BEGIN
    FOR i := FIRST(timea) TO LAST(timea) DO
      WITH t = timea[i],
           n = nodea[i] DO
        IF t >= startTime THEN
          sum := sum + n;
          INC(cnt)
        END
      END
    END;

    RETURN sum / FLOAT(cnt, LONGREAL)
  END MeanValue;

PROCEDURE GetIdx(trace : Trace.T; of : TEXT) : CARDINAL =
  VAR
    res : CARDINAL;
    hadIt := trace.getNodeIdx(of, res);
  BEGIN
    IF NOT hadIt THEN
      Debug.Error(F("GetIdx : \"%s\" not found", of))
    END;
    RETURN res
  END GetIdx;

TYPE
  Config = RECORD
    tech   : Tech;
    tran   : Tran;
    mode   : Mode;
    simu   : Simu;
    corn   : Corn;
    gate   : Gate;
    fanout : CARDINAL := 1;
    volt := 0.0d0;
    temp := 0.0d0;
    nanoseconds : LONGREAL; (* length of sim in ns *)
    timestep : LONGREAL; (* in seconds *)
    
    workDir : Pathname.T;
    createWorkDir : BOOLEAN;
    templatePath : Pathname.T;
    phazz := SET OF Phaz { Phaz.Setup };
    hspiceModelRoot : Pathname.T;
    hspiceModel     : Pathname.T;

    hspiceLibModels : Pathname.T :=
        "/p/hdk/cad/pdk/pdk764_r0.4HP3_22ww20.1/cmi/hspice/cmi/lnx86/64bit";
    (* what is this file? *)

    pdmiLib         : Pathname.T;
    simRoot := DefSimRoot;
    xaPath : Pathname.T := "/p/hdk/cad/xa/S-2021.09-SP2//bin/";

    para : BOOLEAN; (* parasitic simulation yes/no *)
  END;

VAR
  pp := NEW(ParseParams.T).init(Stdio.stderr);
  c : Config;
  extraMap, overrideMap := NEW(TextTextTbl.Default).init();
  
BEGIN
  TRY
    c.createWorkDir := pp.keywordPresent("-C");
    
    IF pp.keywordPresent("-tech") THEN
      c.tech := VAL(Lookup(pp.getNext(), TechNames), Tech);
      c.hspiceModel := Techs[c.tech].hspiceModel;
      c.hspiceModelRoot := Techs[c.tech].hspiceModelRoot;
    END;

    IF pp.keywordPresent("-fo") THEN
      WITH arg = pp.getNextInt() DO
        IF NOT arg IN SupportedFanouts THEN
          Debug.Error(F("Fanout %s not supported", Int(arg)))
        END;
        c.fanout := arg
      END
    END;
    
    IF pp.keywordPresent("-para") THEN
      WITH arg = pp.getNext() DO
        TRY
          c.para := Scan.Bool(arg)
        EXCEPT
          Lex.Error =>
          Debug.Error(F("Lex.Error : -para arg %s not a boolean", arg))
        END
      END
    END;

    IF c.para THEN
      ProcDeadline := ProcDeadline * ParasiticDeadlineMultiplier
    END;

    IF pp.keywordPresent("-tran") THEN
      c.tran := VAL(Lookup(pp.getNext(), TranNames), Tran)
    END;

    IF pp.keywordPresent("-volt") THEN
      c.volt := pp.getNextLongReal()
    END;
    
    IF pp.keywordPresent("-temp") THEN
      c.temp := pp.getNextLongReal()
    END;

    IF pp.keywordPresent("-mode") THEN
      c.mode := VAL(Lookup(pp.getNext(), ModeNames), Mode)
    END;

    IF pp.keywordPresent("-simu") THEN
      c.simu := VAL(Lookup(pp.getNext(), SimuNames), Simu)
    END;

    IF pp.keywordPresent("-corn") THEN
      c.corn := VAL(Lookup(pp.getNext(), CornNames), Corn)
    END;

    IF pp.keywordPresent("-gate") THEN
      c.gate := VAL(Lookup(pp.getNext(), GateNames), Gate)
    END;

    IF pp.keywordPresent("-d") THEN
      c.workDir := pp.getNext()
    END;

    IF pp.keywordPresent("-T") THEN
      c.templatePath := pp.getNext()
    END;

    IF pp.keywordPresent("-deadline") THEN
      ProcDeadline := pp.getNextLongReal()
    END;

    IF pp.keywordPresent("-p") THEN
      c.phazz := SET OF Phaz {};
      
      REPEAT
        c.phazz := c.phazz + SET OF Phaz { VAL(Lookup(pp.getNext(), PhazNames),
                                           Phaz) }
      UNTIL NOT pp.keywordPresent("-p")
    END;

    IF pp.keywordPresent("-all") THEN
      c.phazz := SET OF Phaz { FIRST(Phaz) .. LAST(Phaz) }
    END;

    WHILE pp.keywordPresent("-m") DO
      WITH kk = pp.getNext(),
           vv = pp.getNext() DO
        IF extraMap.put(kk, vv) THEN
          Debug.Error("Multiple mappings for key " & vv)
        END
      END
    END;

    WHILE pp.keywordPresent("-O") DO
      WITH kk = pp.getNext(),
           vv = pp.getNext() DO
        EVAL overrideMap.put(kk, vv)
      END
    END;
    
    pp.skipParsed();
    pp.finish()
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command line")
  END;

  IF c.workDir # NIL THEN
    TRY
      IF c.createWorkDir THEN
        TRY FS.CreateDirectory(c.workDir) EXCEPT ELSE END
      END;
      Process.SetWorkingDirectory(c.workDir)
    EXCEPT
      OSError.E(e) =>
      Debug.Error(F("Couldn't set working directory to \"%s\" : OSError.E : %s",
                    c.workDir, AL.Format(e)))
    END
  END;

  DoCommonSetup(c);
  
  FOR phaz := FIRST(Phaz) TO LAST(Phaz) DO
    IF phaz IN c.phazz THEN
      Debug.Out(F("*****  PHASE %s  ***** ", PhazNames[phaz]));
      Phases[phaz](c)
    END
  END
  
END Main.
