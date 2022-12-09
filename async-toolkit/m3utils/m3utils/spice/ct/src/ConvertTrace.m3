MODULE ConvertTrace EXPORTS Main;

(* ct program -- 

   convert 
          spice trace files (ASCII .tr0 files) 
                    to 
          aspice .names and .trace files

   Aspice output file is created in the fast format (blocked by node)

   Algorithm is external (uses numerous disk files for reordering)

   Author : Mika Nystrom <mika.nystroem@intel.com>

   SCROLL DOWN FOR Usage !

*)

IMPORT FileRd;
IMPORT Rd;
IMPORT Debug;
IMPORT TextSeq;
IMPORT FileWr, Wr;
FROM Fmt IMPORT LongReal, Int, F;
IMPORT FS;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT Pathname;
IMPORT OSError, AL;
IMPORT Thread;
IMPORT Params;
IMPORT TextSet, TextSetDef;
IMPORT Tr0;
IMPORT DataBlock;
IMPORT RegExList;
IMPORT RegEx;
IMPORT CitTextUtils AS TextUtils;
IMPORT Fsdb;
IMPORT TraceFile, FileNamer;
IMPORT TempReader;

<*FATAL Thread.Alerted*>

CONST LR = LongReal;
      
VAR doDebug := Debug.DebugThis("CT");

CONST Usage = "[-fsdb <fsdbPath>] [-compress <compressPath>] [-rename <dutName>] [-scaletime <timeScaleFactor>] [-offsettime <timeOffset>] [-offsetvoltage <voltageOffset>] [-dosources] [-dofiles] [ [-n <nodename>] ...] [-threads <fsdb_threads>] [-wthreads <write_threads>] [-fromworkdir] <inFileName> <outFileRoot>";

(*
  will generate <outFileRoot>.trace and <outFileRoot>.names 

  -fsdb        read FSDB format.  Argument is absolute path
               to FSDB reading program (can be a shell script
               launching via Netbatch).  Usually nanosimrd.

               default format is CSDF if this is not provided

  -compress    Compress waveform data.  Argument is absolute path to 
               compression program (usually spicestream).

               Note that if -fsdb is used, this path needs to be correct
               for the environment in which the FSDB reading program is run.
 
               For example, if nanosimrd is run via Netbatch, the compression
               program needs to be accessible from the Netbatch compute 
               slaves, since it will be launched from within the fsdb
               conversion program.

  -resample    resample data at timestep given

  

  -fromworkdir skips the parsing and generates the 
               trace file from the work directory directly
               NOT YET IMPLEMENTED!   SOON!!

  -notrace     do not generate final .trace output

  -dosources   generate PWL sources for all observed signals

  -threads     how many threads to use for reading FSDB
  
  -wthreads    how many threads to use for writing .trace
               (recommendation is TO NOT USE this option)

  -maxfiles    max # of files to use in temp directory

  -n           restrict attention to nodes mentioned on cmd line

  -r           restrict attention to nodes matching regex

  -workdir     rename working directory ( default : ct.work )

  <inFileName> is name of input file in FSDB or CSDF format


*)


CONST DefMaxFiles = 1000;

VAR maxFiles := DefMaxFiles;
    
CONST 
  MaxMem = 16*1024*1024; (* fit at least one row *)

VAR nFiles : CARDINAL;

PROCEDURE FileRd_Open(fn : Pathname.T) : Rd.T RAISES { OSError.E } =
  BEGIN
    IF doDebug THEN
      Debug.Out(F("opening file \"%s\"", fn));
    END;
    RETURN FileRd.Open(fn)
  END FileRd_Open;

PROCEDURE ReadEntireFile(tempReader   : TempReader.T;
                         idx          : CARDINAL;
                         VAR data     : ARRAY OF LONGREAL)
  RAISES { Rd.Failure, OSError.E } =
  BEGIN
    tempReader.readEntireFile(idx, data)
  END ReadEntireFile;
  
PROCEDURE CreateBuffers(tempReader     : TempReader.T;
                        fnr            : FileNamer.T;
                        VAR time, data : REF ARRAY OF LONGREAL)
  RAISES { OSError.E, Rd.Failure } =
  (* create time and data buffers, and read time into the time buffer *)
  VAR
    aLen : CARDINAL;
  BEGIN
    WITH timeIdx = 0,
         fn      = fnr.name(timeIdx),
         rd      = FileRd_Open(fn) DO
      aLen := DataBlock.DataCount(rd, timeIdx);
      Rd.Close(rd)
    END;

    Debug.Out(F("Creating time/data buffers: %s timesteps",
                Int(aLen)));
    time := NEW(REF ARRAY OF LONGREAL, aLen); (* alloc time *)
    data := NEW(REF ARRAY OF LONGREAL, aLen); (* alloc data *)

    IF doDebug THEN Debug.Out("Reading time data") END;
    
    ReadEntireFile(tempReader, 0, time^);

    IF NUMBER(time^) = 0 THEN
      Debug.Out("Time is empty")
    ELSE
      Debug.Out(F("Time %s -> %s", LR(time[FIRST(time^)]), LR(time[LAST(time^)])))
    END
  END CreateBuffers;
  
PROCEDURE WriteSources(tempReader   : TempReader.T;
                       fnr          : FileNamer.T) =
  (* read data from each file in temp directory and output
     as PWL voltage sources to be used as a stimulus(?) *)
  VAR
    tFn := ofn & ".sources";
    sWr : Wr.T;
    time, data : REF ARRAY OF LONGREAL;
  BEGIN
    TRY
      sWr := FileWr.Open(tFn)
    EXCEPT
      OSError.E(x) => Debug.Error("Unable to open sources file \"" & tFn & "\" for writing : OSError.E : " & AL.Format(x))
    END;

    TRY
      Wr.PutText(sWr, "* sources file generated by ConvertTrace.m3\n");
    EXCEPT
      Wr.Failure(x) => Debug.Error("Write error on sources file : Wr.Failure : " & AL.Format(x))
    END;

    FOR i := 0 TO names.size() - 1 DO
      WITH fn   = fnr.name(i) DO
        TRY
          IF i = 0 THEN
            CreateBuffers(tempReader, fnr, time, data)
          ELSE
              Wr.PutText(sWr, "* source for " & names.get(i) & "\n");
              ReadEntireFile(tempReader, i, data^);
              Wr.PutText(sWr, F("V%s src%s 0 PWL (\n", Int(i), Int(i)));
              FOR i := FIRST(data^) TO LAST(data^) DO
                Wr.PutText(sWr, F("+   %20s       %20s\n",
                                  LongReal(time[i]),
                                  LongReal(data[i])))
              END;
              Wr.PutText(sWr, "+)\n\n");
          END
        EXCEPT
          OSError.E(x) =>
          Debug.Error("Unable to open temp file \"" & fn & "\" for reading : OSError.E : " & AL.Format(x))
        |
          Rd.Failure(x) =>
          Debug.Error("Read error on temp file \"" & fn & "\" for reading : Rd.Failure : " & AL.Format(x))
        |
          Wr.Failure(x) =>
          Debug.Error("Write error on sources file, lately reading \"" & fn & "\" : Wr.Failure : " & AL.Format(x))
        END
      END
    END;
    
    TRY
      Wr.Close(sWr)
    EXCEPT
      Wr.Failure(x) =>
      Debug.Error("Trouble closing sources file : Wr.Failure : " &
        AL.Format(x))
    END
  END WriteSources;

PROCEDURE Unslash(fn : Pathname.T) : Pathname.T =
  BEGIN
    RETURN TextUtils.Replace(fn, "/", "::")
  END Unslash;

PROCEDURE WriteFiles(tempReader : TempReader.T; fnr : FileNamer.T) =
  (* read data from each file in temp directory and output
     in simple ASCII format, one file per signal *)
  VAR
    tDn := ofn & ".files";
    sWr : Wr.T;
    time, data : REF ARRAY OF LONGREAL;
    sFn : Pathname.T;
  BEGIN
    TRY
      FS.CreateDirectory(tDn);
    EXCEPT
    ELSE
    END;
    
    FOR i := 0 TO names.size() - 1 DO
      WITH fn   = fnr.name(i) DO
        TRY
          IF i = 0 THEN
            CreateBuffers(tempReader, fnr, time, data);
          ELSE
            TRY
              sFn := tDn & "/" & Unslash(names.get(i));
              sWr := FileWr.Open(sFn)
            EXCEPT
              OSError.E(x) => Debug.Error("Unable to open file \"" & sFn & "\" for writing : OSError.E : " & AL.Format(x))
            END;
            
            ReadEntireFile(tempReader, i, data^);
            
            FOR i := FIRST(data^) TO LAST(data^) DO
              Wr.PutText(sWr, F("%20s       %20s\n",
                                LongReal(time[i]),
                                LongReal(data[i])))
            END;
            TRY
              Wr.Close(sWr);
            EXCEPT
              Wr.Failure(x) => Debug.Error("Trouble closing sources file "&sFn&" : Wr.Failure : " &
                AL.Format(x))
            END
          END
        EXCEPT
          OSError.E(x) =>
          Debug.Error("Unable to open temp file \"" & fn & "\" for reading : OSError.E : " & AL.Format(x))
        |
          Rd.Failure(x) =>
          Debug.Error("Read error on temp file \"" & fn & "\" for reading : Rd.Failure : " & AL.Format(x))
        |
          Wr.Failure(x) =>
          Debug.Error("Write error on sources file, lately reading \"" & fn & "\" : Wr.Failure : " & AL.Format(x))
        END
      END
    END
  END WriteFiles;

PROCEDURE WriteTrace(fnr : FileNamer.T) =
  BEGIN
    WITH tr = NEW(TraceFile.T).init(ofn, nFiles, names.size(), fnr) DO
      IF wthreads > 1 THEN
        tr.writePll(wthreads, writeTraceCmdPath)
      ELSE
        tr.write()
      END
    END
  END WriteTrace;
  
VAR
  names := NEW(TextSeq.T).init();
  ifn, ofn : Pathname.T;

  wd := "ct.work";
  dutName : TEXT := NIL;
  pp := NEW(ParseParams.T).init(Stdio.stderr);
  timeScaleFactor, voltageScaleFactor := 1.0d0;
  timeOffset, voltageOffset := 0.0d0;
  lNo := 1;
  doSources := FALSE;
  doFiles   := FALSE;

  restrictNodes : TextSet.T := NIL;
  doTrace := TRUE; (* default output *)

  wait := FALSE;

  wrWorkers := 1;
  regExList : RegExList.T := NIL;

  fsdbCmdPath       : Pathname.T := NIL;
  compressCmdPath   : Pathname.T := NIL;
  writeTraceCmdPath : Pathname.T := NIL;

  parseFmt := ParseFmt.Tr0;

  threads  : CARDINAL := 1;
  wthreads : CARDINAL := 1;

  interpolate := Fsdb.NoInterpolate;

TYPE
  ParseFmt = { Tr0, Fsdb };
  
BEGIN
  TRY
    IF    pp.keywordPresent("-rename") THEN
      dutName := pp.getNext()
    END;
    IF pp.keywordPresent("-scaletime") THEN
      timeScaleFactor := pp.getNextLongReal()
    END;

    IF pp.keywordPresent("-resample") OR pp.keywordPresent("-R") THEN
      interpolate := pp.getNextLongReal()
    END;

    IF pp.keywordPresent("-fsdb") THEN
      fsdbCmdPath := pp.getNext();
      parseFmt := ParseFmt.Fsdb;
    END;

    IF pp.keywordPresent("-compress") THEN
      compressCmdPath := pp.getNext()
    END;

    IF pp.keywordPresent("-wrtrace") THEN
      writeTraceCmdPath := pp.getNext()
    END;
    
    IF pp.keywordPresent("-scalevoltage") THEN
      voltageScaleFactor := pp.getNextLongReal()
    END;
    IF pp.keywordPresent("-offsettime") THEN
      timeOffset := pp.getNextLongReal()
    END;
    IF pp.keywordPresent("-offsetvoltage") THEN
      voltageOffset := pp.getNextLongReal()
    END;
    IF pp.keywordPresent("-dosources") THEN
      doSources := TRUE
    END;
    IF pp.keywordPresent("-dofiles") THEN
      doFiles := TRUE
    END;
    IF pp.keywordPresent("-notrace") THEN
      doTrace := FALSE
    END;
    IF pp.keywordPresent("-threads") THEN
      threads := pp.getNextInt()
    END;
    IF pp.keywordPresent("-wthreads") THEN
      wthreads := pp.getNextInt()
    END;
    IF pp.keywordPresent("-maxfiles") THEN
      maxFiles := pp.getNextInt()
    END;

    WHILE pp.keywordPresent("-n") DO
      IF restrictNodes = NIL THEN
        restrictNodes := NEW(TextSetDef.T).init()
      END;
      EVAL restrictNodes.insert(pp.getNext())
    END;

    WHILE pp.keywordPresent("-r") DO
      WITH regExStr = pp.getNext() DO
        TRY
          regExList := RegExList.Cons(RegEx.Compile(regExStr), regExList)
        EXCEPT
          RegEx.Error(x) =>
          Debug.Error(F("Cannot compile regex /%s/ : RegEx.Error : %s",
                        regExStr,
                        x))
        END
      END
    END;
    
    wait := pp.keywordPresent("-w") OR pp.keywordPresent("-wait");

    IF pp.keywordPresent("-wd") OR pp.keywordPresent("-workdir") THEN
      wd := pp.getNext()
    END;

    IF pp.keywordPresent("-wrworkers") THEN
      wrWorkers := pp.getNextInt()
    END;

    pp.skipParsed();
    
    ifn := pp.getNext();
    ofn := pp.getNext();
    
    pp.finish();
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command-line parameters\nUsage: " & Params.Get(0) & " " & Usage)
  END;

  IF wthreads # 1 THEN
    Debug.Warning("wthreads > 1 not recommended")
  END;
  
  TRY FS.CreateDirectory(wd) EXCEPT ELSE END;

  TRY

    IF doDebug THEN
      Debug.Out("ConvertTrace parsing...");
    END;
    CASE parseFmt OF
      ParseFmt.Tr0 =>

      VAR
        rd : Rd.T;
      BEGIN
        TRY
          rd  := FileRd_Open(ifn)
        EXCEPT
          OSError.E(x) => Debug.Error("Trouble opening input file \"" & ifn & "\": OSError.E : " & AL.Format(x))
        END;
        
        names.addhi("TIME");
        Tr0.Parse(wd,
                  ofn,
                  names,
                  maxFiles,
                  nFiles,
                  MaxMem,
                  timeScaleFactor,
                  timeOffset,
                  voltageScaleFactor,
                  voltageOffset,
                  dutName,
                  rd,
                  wait,
                  restrictNodes,
                  regExList);

        TRY Rd.Close(rd) EXCEPT ELSE END
      END
    |
      ParseFmt.Fsdb =>
      Fsdb.Parse(wd,
                 ofn,
                 names,
                 maxFiles,
                 nFiles,
                 timeScaleFactor,
                 timeOffset,
                 voltageScaleFactor,
                 voltageOffset,
                 dutName,
                 ifn,
                 wait,
                 restrictNodes,
                 regExList,
                 fsdbCmdPath,
                 compressCmdPath,
                 threads,
                 interpolate)
    END;
    IF doDebug THEN
      Debug.Out("ConvertTrace parsing done.")
    END
    
  EXCEPT
    Tr0.SyntaxError(e) => Debug.Error("Syntax error on line " & Int(lNo) & " : " &
      e)
  |
    Tr0.ShortRead => Debug.Warning("Tr0.ShortRead: Short read on final line, data may be corrupted")
  |
    Rd.Failure(x) => Debug.Error("Trouble reading input file : Rd.Failure : " & AL.Format(x))
  END;


  WITH fnr = NEW(FileNamer.T).init(wd, nFiles, names.size()) DO
    IF doTrace THEN
      WriteTrace(fnr)
    END;

    VAR
      tempReader := NEW(TempReader.T).init(fnr);
    BEGIN
    
      IF doSources THEN
        WriteSources(tempReader, fnr)
      END;
      
      IF doFiles THEN
        WriteFiles(tempReader, fnr)
      END
    END
  END

END ConvertTrace.
