MODULE ConvertTrace EXPORTS Main;

(* ct program -- 

   convert 
          spice trace files (ASCII .tr0 files) 
                    to 
          aspice .names and .trace files

   Aspice output file is created in the fast format (blocked by node)

   Algorithm is external (uses numerous disk files for reordering)

   Author : Mika Nystrom <mika.nystroem@intel.com>

   ct [-rename <dutName>] [-scaletime <timeScaleFactor>] [-offsettime <timeOffset>] [-offsetvoltage <voltageOffset>] [-dosources] [-dofiles] [ [-n <nodename>] ...] <inFileName> <outFileRoot>

   will generate <outFileRoot>.trace and <outFileRoot>.names

*)

IMPORT FileRd;
IMPORT Rd;
IMPORT Debug;
IMPORT TextSeq;
IMPORT FileWr, Wr;
IMPORT Math;
FROM Fmt IMPORT LongReal, Int, F;
IMPORT FS;
IMPORT UnsafeWriter;
IMPORT Time;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT Pathname;
IMPORT OSError, AL;
IMPORT Thread;
IMPORT Params;
IMPORT TextSet, TextSetDef;
IMPORT Tr0; FROM Tr0 IMPORT FileIndex;
IMPORT DataBlock;
IMPORT Text;

<*FATAL Thread.Alerted*>

CONST LR = LongReal;
      TE = Text.Equal;
      
VAR doDebug := Debug.DebugThis("CT");

CONST Usage = "[-rename <dutName>] [-scaletime <timeScaleFactor>] [-offsettime <timeOffset>] [-offsetvoltage <voltageOffset>] [-dosources] [-maxfiles <nfiles>] <inFileName> <outFileRoot>";

CONST DefMaxFiles = 1000;

VAR maxFiles := DefMaxFiles;
    
CONST 
  MaxMem = 16*1024*1024; (* fit at least one row *)

PROCEDURE FormatFN(i : CARDINAL) : TEXT =
  BEGIN RETURN F("%08s", Int(i)) END FormatFN;

VAR nFiles : CARDINAL;

PROCEDURE WriteTrace() =
  (* read data from each file in temp directory and output
     in reordered trace format for fast aplot access *)
  VAR
    tFn := ofn & ".trace";
    tWr : Wr.T;
    time, data : REF ARRAY OF LONGREAL;
  BEGIN
    Debug.Out("WriteTrace");
    TRY
      tWr := FileWr.Open(tFn)
    EXCEPT
      OSError.E(x) => Debug.Error("Unable to open trace file \"" & tFn & "\" for writing : OSError.E : " & AL.Format(x))
    END;

    Debug.Out("WriteTrace writing header...");
    TRY
      UnsafeWriter.WriteI(tWr, 1);
      (* this tells aplot it is the reordered format *)
      
      UnsafeWriter.WriteI(tWr, TRUNC(Time.Now()));
      (* timestamp *)
      
      UnsafeWriter.WriteI(tWr, names.size());
      (* number of nodes.  
         aplot computes the offsets based on the file length? *)
      
    EXCEPT
      Wr.Failure(x) =>
      Debug.Error("Write error writing header of trace file : Wr.Failure : " & AL.Format(x))
    END;
    
    Debug.Out("WriteTrace walking names...");
    FOR i := 0 TO names.size() - 1 DO
      TRY
        IF i = 0 THEN 
          Debug.Out("WriteTrace creating buffers...");
          CreateBuffers(time, data);
          Debug.Out(F("WriteTrace writing TIME (%s values)...",
                      Int(NUMBER(time^))));
          UnsafeWriter.WriteLRA(tWr, time^);
        ELSIF restrictNodes = NIL OR restrictNodes.member(names.get(i)) THEN
          ReadEntireFile(i, data^);
          UnsafeWriter.WriteLRA(tWr, data^);
          Rd.Close(rd)
        END
      EXCEPT
        OSError.E(x) =>
        Debug.Error("Unable to open temp file \"" & FileName(i) & "\" for reading : OSError.E : " & AL.Format(x))
      |
        Rd.Failure(x) =>
        Debug.Error("Read error on temp file \"" & FileName(i) & "\" for reading : Rd.Failure : " & AL.Format(x))
      |
        Wr.Failure(x) =>
        Debug.Error("Write error on trace file, lately reading \"" & FileName(i) & "\" : Wr.Failure : " & AL.Format(x))
      END
    END;

    TRY
      Wr.Close(tWr)
    EXCEPT
      Wr.Failure(x) => Debug.Error("Trouble closing trace file : Wr.Failure : " &
        AL.Format(x))
    END
  END WriteTrace;

PROCEDURE FileRd_Open(fn : Pathname.T) : Rd.T RAISES { OSError.E } =
  BEGIN
    Debug.Out(F("opening file \"%s\"", fn));
    RETURN FileRd.Open(fn)
  END FileRd_Open;

TYPE
  FileReader = OBJECT
    dbRd : Rd.T       ;
    dbDb : DataBlock.T;
    dbFn : Pathname.T ;
  METHODS
    init() : FileReader := InitM;

    readEntireFile(idx : CARDINAL; VAR data : ARRAY OF LONGREAL)
      RAISES { Rd.Failure, OSError.E } := ReadEntireFileM;
  END;

PROCEDURE InitM(self : FileReader) : FileReader =
  BEGIN
    self.dbRd := NIL;
    self.dbDb := NIL;
    self.dbFn := NIL;
    RETURN self
  END InitM;

PROCEDURE ReadEntireFileM(self     : FileReader;
                          idx      : CARDINAL;
                          VAR data : ARRAY OF LONGREAL)
  RAISES { Rd.Failure, OSError.E } =
  VAR
    fn := FileName(idx);
    ptr := 0;
    aLen := NUMBER(data);

  BEGIN
    IF idx = 0 THEN
      TRY
        WITH rd  = FileRd_Open(fn),
             cnt = DataBlock.ReadData(rd, idx, data) DO
          <*ASSERT cnt = NUMBER(data)*>
          Rd.Close(rd)
        END
      EXCEPT
        Rd.EndOfFile => <*ASSERT FALSE*> (* internal program error *)
      END
    ELSE
      IF self.dbDb = NIL OR NOT TE(fn, self.dbFn) THEN
        IF self.dbRd # NIL THEN
          Rd.Close(self.dbRd);
        END;
        self.dbFn := fn;
        self.dbRd := FileRd_Open(fn);
        self.dbDb := NEW(DataBlock.T).init(self.dbRd, aLen)
      END;
      
      <*ASSERT self.dbDb.haveTag(idx)*>
      WITH readRecs = self.dbDb.readData(idx, data) DO
        IF readRecs # aLen THEN
          Debug.Warning(F("ConvertTrace.ReadEntireFileM: short read for %s (%s) : %s # %s",
                          names.get(idx), Int(idx), Int(ptr), Int(aLen)))
        END
      END
    END
  END ReadEntireFileM;

VAR
  singleReader := NEW(FileReader).init();

PROCEDURE ReadEntireFile(idx : CARDINAL; VAR data : ARRAY OF LONGREAL)
  RAISES { Rd.Failure, OSError.E } =
  BEGIN
    singleReader.readEntireFile(idx, data)
  END ReadEntireFile;
  
PROCEDURE FileName(idx : CARDINAL) : TEXT =
  BEGIN
    RETURN wd & "/" & FormatFN(FileIndex(nFiles, names.size(), idx))
  END FileName;

PROCEDURE CreateBuffers(VAR time, data : REF ARRAY OF LONGREAL)
  RAISES { OSError.E, Rd.Failure } =
  (* create time and data buffers, and read time into the time buffer *)
  VAR
    aLen : CARDINAL;
  BEGIN
    WITH timeIdx = 0,
         fn      = FileName(timeIdx),
         rd      = FileRd_Open(fn) DO
      aLen := DataBlock.DataCount(rd, timeIdx);
      Rd.Close(rd)
    END;
    
    Debug.Out(F("Creating time/data buffers: %s timesteps",
                Int(aLen)));
    time := NEW(REF ARRAY OF LONGREAL, aLen); (* alloc time *)
    data := NEW(REF ARRAY OF LONGREAL, aLen); (* alloc data *)

    Debug.Out("Reading time data");
    
    ReadEntireFile(0, time^);

    IF NUMBER(time^) = 0 THEN
      Debug.Out("Time is empty")
    ELSE
      Debug.Out(F("Time %s -> %s", LR(time[FIRST(time^)]), LR(time[LAST(time^)])))
    END
  END CreateBuffers;
  
PROCEDURE WriteSources() =
  (* read data from each file in temp directory and output
     in reordered trace format for fast aplot access *)
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
      WITH fn   = FileName(i) DO
        TRY
          IF i = 0 THEN
            CreateBuffers(time, data)
          ELSIF restrictNodes = NIL OR restrictNodes.member(names.get(i)) THEN
              Wr.PutText(sWr, "* source for " & names.get(i) & "\n");
              ReadEntireFile(i, data^);
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

PROCEDURE WriteFiles() =
  (* read data from each file in temp directory and output
     in simple ASCII format *)
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
      WITH fn   = FileName(i) DO
        TRY
          IF i = 0 THEN
            CreateBuffers(time, data);
          ELSIF restrictNodes = NIL OR restrictNodes.member(names.get(i)) THEN
              TRY
                sFn := tDn & "/" & names.get(i);
                sWr := FileWr.Open(sFn)
              EXCEPT
                OSError.E(x) => Debug.Error("Unable to open file \"" & sFn & "\" for writing : OSError.E : " & AL.Format(x))
              END;

              ReadEntireFile(i, data^);

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

VAR
  names := NEW(TextSeq.T).init();
  ifn, ofn : Pathname.T;

  rd  : Rd.T;

  lbuff : REF ARRAY OF ARRAY OF LONGREAL;
  lbp : CARDINAL := 0; lbq : CARDINAL := 0;

  Exp : ARRAY [-300..300] OF LONGREAL;
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
  
BEGIN
  TRY
    IF    pp.keywordPresent("-rename") THEN
      dutName := pp.getNext()
    END;
    IF pp.keywordPresent("-scaletime") THEN
      timeScaleFactor := pp.getNextLongReal()
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
    IF pp.keywordPresent("-maxfiles") THEN
      maxFiles := pp.getNextInt()
    END;
    WHILE pp.keywordPresent("-n") DO
      IF restrictNodes = NIL THEN
        restrictNodes := NEW(TextSetDef.T).init()
      END;
      EVAL restrictNodes.insert(pp.getNext())
    END;

    wait := pp.keywordPresent("-w") OR pp.keywordPresent("-wait");

    IF pp.keywordPresent("-wrworkers") THEN
      wrWorkers := pp.getNextInt()
    END;
    
    ifn := pp.getNext();
    ofn := pp.getNext();
    
    pp.finish();
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command-line parameters\nUsage: " & Params.Get(0) & " " & Usage)
  END;

  TRY
    rd  := FileRd_Open(ifn)
  EXCEPT
    OSError.E(x) => Debug.Error("Trouble opening input file \"" & ifn & "\": OSError.E : " & AL.Format(x))
  END;
  
  names.addhi("TIME");
  TRY FS.CreateDirectory(wd) EXCEPT ELSE END;

  FOR i := FIRST(Exp) TO LAST(Exp) DO
    Exp[i] := Math.pow(10.0d0,FLOAT(i,LONGREAL))
  END;

  TRY

    Debug.Out("ConvertTrace parsing...");
    Tr0.Parse(wd,
              ofn,
              names,
              maxFiles,
              nFiles,
              MaxMem,
              lbp,
              lbq,
              lbuff,
              timeScaleFactor,
              timeOffset,
              voltageScaleFactor,
              voltageOffset,
              dutName,
              rd,
              wait);
    Debug.Out("ConvertTrace parsing done.")
    
  EXCEPT
    Tr0.SyntaxError(e) => Debug.Error("Syntax error on line " & Int(lNo) & " : " &
      e)
  |
    Tr0.ShortRead => Debug.Warning("Tr0.ShortRead: Short read on final line, data may be corrupted")
  |
    Rd.Failure(x) => Debug.Error("Trouble reading input file : Rd.Failure : " & AL.Format(x))
  END;


  IF doTrace THEN
    WriteTrace()
  END;

  IF doSources THEN
    WriteSources()
  END;

  IF doFiles THEN
    WriteFiles()
  END

END ConvertTrace.
