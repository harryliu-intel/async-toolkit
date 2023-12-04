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
IMPORT Debug;
IMPORT Stdio;
FROM Fmt IMPORT F, Int; IMPORT Fmt;
IMPORT OSError;
IMPORT AL;
IMPORT Process;
IMPORT Math;
IMPORT FS;
IMPORT Scan;
IMPORT Lex;
FROM TechSetup IMPORT extraMap, overrideMap;
IMPORT TechSetup;
IMPORT TechConvert;
IMPORT TechSimulate;
IMPORT TechProgress;
IMPORT TechMeasure;

FROM TechLookup IMPORT Lookup;
FROM TechConfig IMPORT Tech, Tran, Mode, Phaz, Simu, Corn, Gate;
FROM TechConfig IMPORT TranNames, ModeNames, PhazNames, SimuNames, CornNames,
                       GateNames, TechNames;
FROM TechConfig IMPORT SupportedFanouts;
FROM TechTechs IMPORT Techs;
FROM TechCleanup IMPORT DeleteMatching, DeleteRecursively, CompressFilesWithExtension;

IMPORT TechConfig;
IMPORT P1278p3TechProcess;
IMPORT Text;

TYPE Config = TechConfig.T;

CONST ParasiticDeadlineMultiplier = 2.0d0;

      TE = Text.Equal;


CONST LR = Fmt.LongReal;

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
  
  
CONST
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
  
TYPE
  RunPhase = PROCEDURE(READONLY c : Config);

CONST
  Phases = ARRAY Phaz OF RunPhase { TechSetup.DoSetup,
                                    TechSimulate.DoSimulate,
                                    DoConvertPhaz,
                                    DoClean,
                                    DoMeasurePhaz
  };
  
CONST CornDelay = ARRAY Corn OF LONGREAL { 1.0d0, 3.0d0, 0.8d0, 2.0d0, 2.0d0 };

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
                           ParaNanoFactor[c.para] * 40.0d0 * (delayFactor + 1.5d0),
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

PROCEDURE DoConvertPhaz(READONLY c : Config) =
  BEGIN
    EVAL TechConvert.DoConvert(c, c.simRoot, exitOnError := TRUE)
  END DoConvertPhaz;

PROCEDURE DoClean(READONLY c : Config) =
  BEGIN
    IF Verbose THEN
      Debug.Out("DoClean()")
    END;
    TRY
      WITH fsdbPath = TechConvert.FindFsdbInDir(c.workDir) DO
        IF fsdbPath # NIL THEN
          IF Verbose THEN
            Debug.Out("DoClean deleting " & fsdbPath)
          END;
          FS.DeleteFile(fsdbPath) 
        END
      END
    EXCEPT ELSE END;
    
    DeleteRecursively(c.workDir, c.simRoot & ".ctwork");
    DeleteRecursively(c.workDir, "progress.ctwork");
    DeleteRecursively(c.workDir, "ct.work");
    DeleteMatching   (c.workDir, c.simRoot & ".mc");
    
    CompressFilesWithExtension(c.workDir, ".lis");
    CompressFilesWithExtension(c.workDir, ".ic0");
  END DoClean;

PROCEDURE DoMeasurePhaz(READONLY c : Config) =
  BEGIN
    IF NOT (TechMeasure.DoMeasure(c, c.simRoot, "measure.dat", c.workDir, FALSE) OR
            TechMeasure.DoMeasure(c, TechProgress.Root, "measure.dat", c.workDir))
     THEN Debug.Error("Measure phase failed : no measurement available")
    END
  END DoMeasurePhaz;
  

VAR
  pp := NEW(ParseParams.T).init(Stdio.stderr);
  c : Config;
  
BEGIN
  TRY
    c.createWorkDir := pp.keywordPresent("-C");
    
    IF pp.keywordPresent("-gate") THEN
      c.gate := VAL(Lookup(pp.getNext(), GateNames), Gate)
    END;

    CASE c.gate OF
      Gate.Xor_Z1, Gate.Xor_Z2, Gate.Xor_Z3,
      Gate.Xor_Z6, Gate.Xor_Z9, Gate.Xor_Z12, Gate.Xor_Z18 =>
      IF pp.keywordPresent("-stdcells") THEN
        WITH sc = VAL(Lookup(pp.getNext(), P1278p3TechProcess.StdcellNames),
                      P1278p3TechProcess.Stdcells) DO
          c.stdcells := P1278p3TechProcess.StdcellNames[sc]
        END
      END
    ELSE
    END;

    IF pp.keywordPresent("-tech") THEN
      c.tech := VAL(Lookup(pp.getNext(), TechNames), Tech);

      IF c.tech = Tech.P1278p3 AND TE(c.stdcells, "i0m") THEN
        c.tech := Tech.P1278p3_i0m
      END;

      c.hspiceModel := Techs[c.tech].hspiceModel;
      c.hspiceModelRoot := Techs[c.tech].hspiceModelRoot;
    END;

    IF pp.keywordPresent("-hspicemodelroot") THEN
      c.hspiceModelRoot := pp.getNext()
    END;

    IF pp.keywordPresent("-fo") THEN
      WITH arg = pp.getNextInt() DO
        IF NOT arg IN SupportedFanouts THEN
          Debug.Error(F("Fanout %s not supported", Int(arg)))
        END;
        c.fanout := arg
      END
    END;

    IF pp.keywordPresent("-sigma") THEN
      c.sigma := pp.getNextLongReal()
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
      TechSimulate.ProcDeadline :=
          TechSimulate.ProcDeadline * ParasiticDeadlineMultiplier
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

    IF pp.keywordPresent("-loadcap") THEN
      c.loadCap := pp.getNextLongReal()
    ELSE
      c.loadCap := 0.0d0
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


    IF pp.keywordPresent("-d") THEN
      c.workDir := pp.getNext()
    END;

    IF pp.keywordPresent("-T") THEN
      c.templateDir := pp.getNext()
    END;

    IF pp.keywordPresent("-deadline") THEN
      TechSimulate.ProcDeadline := pp.getNextLongReal()
    END;

    IF pp.keywordPresent("-hspicemodelroot") THEN
      c.hspiceModelRoot := pp.getNext();
      c.hspiceModelName := pp.getNext()
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
