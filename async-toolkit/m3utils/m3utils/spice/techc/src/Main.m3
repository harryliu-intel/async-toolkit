MODULE Main;

IMPORT ParseParams;
IMPORT Text;
IMPORT Debug;
IMPORT Stdio;
FROM Fmt IMPORT F, Int; IMPORT Fmt;
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

<*FATAL Thread.Alerted*>

CONST TE = Text.Equal;
      LR = Fmt.LongReal;

TYPE
  Tech = { N3, P1276p4 };
  Tran = { Elvt, Ulvt, Lvt, Svt, Hvt };
  Mode = { Dyn, Leak };
  Phaz = { Setup, Simulate, Convert, Measure };
  Simu = { Xa, Hspice };
  
CONST
  TechNames = ARRAY Tech OF TEXT { "n3"  ,  "1276.4" };
  TranNames = ARRAY Tran OF TEXT { "elvt",  "ulvt", "lvt", "svt", "hvt" };
  ModeNames = ARRAY Mode OF TEXT { "dyn" ,  "leak" };
  PhazNames = ARRAY Phaz OF TEXT { "setup", "simulate", "convert", "measure" };
  SimuNames = ARRAY Simu OF TEXT { "xa",    "hspice" };

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
  Phases = ARRAY Phaz OF RunPhase { DoSetup, DoSimulate, DoConvert, DoMeasure };
  
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

<*NOWARN*>PROCEDURE MapTechN3(READONLY c : Config; map : TextTextTbl.T) =
  BEGIN
  END MapTechN3;

<*NOWARN*>PROCEDURE MapTech1276p4(READONLY c : Config; map : TextTextTbl.T) =
  BEGIN
  END MapTech1276p4;

TYPE Mapper = PROCEDURE(READONLY c : Config; map : TextTextTbl.T);
     
CONST MapTech = ARRAY Tech OF Mapper { MapTechN3, MapTech1276p4 };

PROCEDURE MapCommon(READONLY c : Config;  map : TextTextTbl.T)=
  VAR
    iter : TextTextTbl.Iterator;
    k, v : TEXT;
  BEGIN
    EVAL map.put("@HSPICE_MODEL_ROOT@", c.hspiceModelRoot);
    EVAL map.put("@HSPICE_MODEL@", c.hspiceModel);
    EVAL map.put("@TEMP@", LR(c.temp));
    EVAL map.put("@VOLT@", LR(c.volt));
    WITH nanoseconds = 10.0d0 + 10.0d0 / (c.volt * c.volt + 0.001d0) DO
      EVAL map.put("@NANOSECONDS@", LR(nanoseconds))
    END;
    EVAL map.put("@OPTIONS@", SimOptions[c.simu]);
    EVAL map.put("@CORNER@", c.corner);

    CASE c.mode OF
      Mode.Dyn =>
      EVAL map.put("@RESET_SOURCE@",
                   "Vres _RESET 0 DC=0 PWL 0 0 10ns 0 10.1ns vtrue")
    |
      Mode.Leak =>
      EVAL map.put("@RESET_SOURCE@", "Vres _RESET 0 DC=0")
    END;

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
      Debug.Out(F("k %s -> v %s", k, Debug.UnNil(v)))
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
      
PROCEDURE DoSetup(READONLY c : Config) =
  VAR
    SimFile := c.simRoot & ".sp";
    map     := NEW(TextTextTbl.Default).init();
    template : TextSeq.T;
  BEGIN
    MapCommon(c, map);
    MapTech[c.tech](c, map);

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

PROCEDURE DoSimulate(READONLY c : Config) =
  <*FATAL OSError.E*> (* this pertains to the TextWr *)
  VAR
    wr := NEW(TextWr.T).init();
    stdout, stderr := ProcUtils.WriteHere(wr);

    cmd := F("xa %s.sp -o %s", c.simRoot, c.simRoot);
  BEGIN
    (*Wr.Close(wrIn);*)
    CASE c.simu OF
      Simu.Xa =>
      WITH c = ProcUtils.RunText(cmd,
                                 stdout := stdout,
                                 stderr := stderr,
                                 stdin  := NIL)
       DO
        TRY
          c.wait()
        EXCEPT
          ProcUtils.ErrorExit(err) =>
          Debug.Error(F("command \"%s\" raised ErrorExit : %s",
                        cmd, ProcUtils.FormatError(err)))
        END
      END
    |
      Simu.Hspice => <*ASSERT FALSE*>
    END;
    Debug.Out("DoSimulate output :\n" & TextWr.ToText(wr))
  END DoSimulate;

PROCEDURE DoConvert(READONLY c : Config) =
  <*FATAL OSError.E*> (* this pertains to the TextWr *)
  VAR
    wr := NEW(TextWr.T).init();
    stdout, stderr := ProcUtils.WriteHere(wr);

    cmd := F("/nfs/site/disks/zsc3_fon_fe_0001/mnystroe/m3utils/spice/ct/AMD64_LINUX/ct -fsdb /nfs/site/disks/zsc3_fon_fe_0001/mnystroe/m3utils/spice/fsdb/src/nanosimrd -threads 4 -wthreads 1 -R 50e-12 %s.fsdb %s",
             c.simRoot, c.simRoot);
  BEGIN 
    (*Wr.Close(wrIn);*)
    CASE c.simu OF
      Simu.Xa =>
      WITH c = ProcUtils.RunText(cmd,
                                 stdout := stdout,
                                 stderr := stderr,
                                 stdin  := NIL) DO
        TRY
          c.wait()
        EXCEPT
          ProcUtils.ErrorExit(err) =>
          Debug.Error(F("command \"%s\" raised ErrorExit : %s",
                        cmd, ProcUtils.FormatError(err)))
        END
      END
    |
      Simu.Hspice => <*ASSERT FALSE*>
    END;
    Debug.Out("DoConvert output :\n" & TextWr.ToText(wr))
  END DoConvert;

PROCEDURE DoMeasure(READONLY c : Config) =
  VAR
    trace : Trace.T;
    nSteps : CARDINAL;
    timeData, nodeData : REF ARRAY OF LONGREAL;
    
  BEGIN
    TRY
      trace := NEW(Trace.T).init(DefSimRoot);
    EXCEPT
      OSError.E(x) =>
      Debug.Error("OSError.E reading trace/names file : " & AL.Format(x))
    |
      Rd.Failure(x) =>
      Debug.Error("I/O error reading trace/names file : " & AL.Format(x))
    |
      Rd.EndOfFile =>
      Debug.Error("Short read reading trace/names file")
    END;

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
      StartTran = 2;
    VAR
      xIdx := GetIdx(trace, "x[0]");
      iIdx := GetIdx(trace, "vissx");
      cycle, meancurrent : LONGREAL;
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
      
      Debug.Out("Measured mean current " & LR(meancurrent));
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
    tech : Tech;
    tran : Tran;
    mode : Mode;
    simu : Simu;
    volt := 0.0d0;
    temp := 0.0d0;
    workDir : Pathname.T;
    templatePath : Pathname.T;
    phazz := SET OF Phaz { Phaz.Setup };
    corner := "tttt";
    hspiceModelRoot : Pathname.T;
    hspiceModel     : Pathname.T;
    hspDir          : Pathname.T;
    hspiceLibModels : Pathname.T;
    pdmiLib         : Pathname.T;
    simRoot := DefSimRoot;
  END;

VAR
  pp := NEW(ParseParams.T).init(Stdio.stderr);
  c : Config;
  extraMap, overrideMap := NEW(TextTextTbl.Default).init();
  
BEGIN
  TRY
    IF pp.keywordPresent("-tech") THEN
      c.tech := VAL(Lookup(pp.getNext(), TechNames), Tech)
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

    IF pp.keywordPresent("-d") THEN
      c.workDir := pp.getNext()
    END;

    IF pp.keywordPresent("-T") THEN
      c.templatePath := pp.getNext()
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
      Process.SetWorkingDirectory(c.workDir)
    EXCEPT
      OSError.E(e) =>
      Debug.Error(F("Couldn't set working directory to \"%s\" : OSError.E : %s",
                    c.workDir, AL.Format(e)))
    END
  END;

  FOR phaz := FIRST(Phaz) TO LAST(Phaz) DO
    IF phaz IN c.phazz THEN
      Debug.Out(F("*****  PHASE %s  ***** ", PhazNames[phaz]));
      Phases[phaz](c)
    END
  END
  
END Main.
