MODULE TraceFile;
IMPORT Wr;
IMPORT Debug;
FROM Fmt IMPORT Int, F, LongReal, FN, Bool;
IMPORT OSError;
IMPORT AL;
IMPORT UnsafeWriter;
IMPORT Time;
IMPORT Rd;
IMPORT FileWr;
IMPORT Pathname;
IMPORT FileRd;
IMPORT DataBlock;
IMPORT FileNamer;
IMPORT TempReader;
IMPORT Thread;
IMPORT FS;
IMPORT File;
IMPORT RegularFile;
IMPORT ProcUtils;
IMPORT UnsafeReader;
IMPORT ZtraceFile;
IMPORT SpiceCompress;
IMPORT TempDataRep;
IMPORT ArithConstants;
IMPORT ZtraceNodeHeader;

<*FATAL Thread.Alerted*>

CONST LR = LongReal;

VAR doDebug := Debug.DebugThis("TraceFile");

CONST
  DefTargMaxDev = 0.01d0; (* used when compressed format requested, but only
                             uncompressed available.  

                             Not normally used since nanosimrd compresses 
                             on the fly 
                          *)

PROCEDURE FileRd_Open(fn : Pathname.T) : Rd.T RAISES { OSError.E } =
  BEGIN
    IF doDebug THEN
      Debug.Out(F("opening file \"%s\"", fn));
    END;
    RETURN FileRd.Open(fn)
  END FileRd_Open;

REVEAL
  T = Public BRANDED Brand OBJECT
    ofn    : Pathname.T;
    nFiles : CARDINAL;
    nNames : CARDINAL;
    fnr    : FileNamer.T;
    reader : TempReader.T;
  OVERRIDES
    init     := Init;
    write    := Write;
    writePll := WritePll;
  END;

PROCEDURE Init(t      : T;
               ofn    : Pathname.T;
               nFiles : CARDINAL;
               nNames : CARDINAL;
               fnr    : FileNamer.T) : T =
  BEGIN
    t.ofn    := ofn;
    t.nFiles := nFiles;
    t.nNames := nNames;
    t.fnr    := fnr;
    t.reader := NEW(TempReader.T).init(fnr);
    RETURN t
  END Init;
  
PROCEDURE CreateBuffers(t : T; VAR time, data : REF ARRAY OF LONGREAL)
  RAISES { OSError.E, Rd.Failure } =
  (* create time and data buffers, and read time into the time buffer *)
  VAR
    aLen : CARDINAL;
  BEGIN
    WITH timeIdx = 0,
         fn      = t.fnr.name(timeIdx),
         rd      = FileRd_Open(fn) DO
      aLen := DataBlock.DataCount(rd, timeIdx);
      Rd.Close(rd)
    END;

    Debug.Out(F("Creating time/data buffers: %s timesteps",
                Int(aLen)));
    time := NEW(REF ARRAY OF LONGREAL, aLen); (* alloc time *)
    data := NEW(REF ARRAY OF LONGREAL, aLen); (* alloc data *)

    IF doDebug THEN Debug.Out("Reading time data") END;
    
    t.reader.readEntireFile(0, time^);

    IF NUMBER(time^) = 0 THEN
      Debug.Out("Time is empty")
    ELSE
      Debug.Out(F("Time %s -> %s", LR(time[FIRST(time^)]), LR(time[LAST(time^)])))
    END
  END CreateBuffers;

PROCEDURE Write(t : T; fmt : Version) =
  VAR
    tFn := t.ofn & "." & VersionSuffixes[fmt];
    tWr : Wr.T;
  BEGIN
    Debug.Out(F("TraceFile.Write(%s) -> %s", VersionNames[fmt], tFn));
    
    TRY
      tWr := FileWr.Open(tFn)
    EXCEPT
      OSError.E(x) => Debug.Error("Unable to open trace file \"" & tFn & "\" for writing : OSError.E : " & AL.Format(x))
    END;
    
    TRY
      CASE fmt OF
        Version.Reordered =>
        WriteReordered(t, tWr)
      |
        Version.Unreordered =>
        Debug.Error("Unreordered writing not supported")
      |
        Version.CompressedV1 =>
        WriteCompressedV1(t, tWr)
      |
        Version.Modifying =>
        (* this code doesn't support modifying, at least not like this *)
        <*ASSERT FALSE*>
      END
    EXCEPT
      WriteError(x) =>
      Debug.Error(F("Writing trace file \"%s\", format %s : error : %s",
                    tFn, VersionNames[fmt], x))
    END;

    TRY
      Wr.Close(tWr)
    EXCEPT
      Wr.Failure(x) => Debug.Error("Trouble closing trace file : Wr.Failure : " &
        AL.Format(x))
    END
  END Write;

EXCEPTION WriteError(TEXT);

PROCEDURE WriteTimeTrace(wr            : Wr.T;
                         READONLY time : ARRAY OF LONGREAL;
                         VAR pos       : CARDINAL) : ZtraceNodeHeader.T
  RAISES { Wr.Failure } =
  CONST
    MaxRelDelta = 1.0d-6; (* 1 p.p.m. max time error permitted *)
  VAR
    opos := Wr.Index(wr);
    first, last, lf : LONGREAL;
    ok := TRUE;
    dir : ZtraceNodeHeader.T;
  BEGIN
    (* see if we can write time as compressed format or not *)

    first := time[0];
    last  := time[LAST(time)];
    lf    := FLOAT(LAST(time), LONGREAL);

    IF last = 0.0d0 THEN
      ok := FALSE
    ELSE
      FOR i := 0 TO LAST(time) DO
        WITH ratio    = FLOAT(i, LONGREAL) / lf,
             pred     = (last - first) * ratio,
             reldelta = ABS((pred - time[i])/last) DO
          IF reldelta > MaxRelDelta THEN
            ok := FALSE;
            EXIT
          END
        END
      END
    END;

    Debug.Out("TraceFile.WriteTimeTrace: compression ok = " & Bool(ok));
    
    IF ok THEN
      dir.code  := ArithConstants.LinearCode;
      dir.norm  := SpiceCompress.Norm { min := first, max := last };
    ELSE
      (* write uncompressed time data *)
    
      dir.code  := ArithConstants.DenseCode;
      UnsafeWriter.WriteLRAAt(wr, time, pos);
      dir.norm  := SpiceCompress.IllegalNorm;
    END;

    (* update directory for time data *)
    pos := Wr.Index(wr);
    Debug.Out(F("WriteTimeTrace : opos %s pos %s code %s",
                Int(opos), Int(pos), Int(dir.code)));
    
    dir.bytes := pos - opos;
    dir.start := opos;

    RETURN dir
    
  END WriteTimeTrace;
  
PROCEDURE WriteCompressedV1(t : T; tWr : Wr.T)
  RAISES { WriteError } =
  CONST
    fmt = Version.CompressedV1;
  VAR
    header := Header { fmt, Time.Now(), t.nNames };
    dir    := NEW(REF ZtraceFile.Directory).init(t.nNames);
    z      : ZtraceFile.Metadata;
    time, data    : REF ARRAY OF LONGREAL;
  BEGIN
    Debug.Out("TraceFile.WriteCompressedV1");

    TRY
      CreateBuffers(t, time, data)
    EXCEPT
      OSError.E(x) => Debug.Error("WriteCompressedV1 opening input : OSError.E : " & AL.Format(x))
    |
      Rd.Failure(x) => Debug.Error("WriteCompressedV1 opening input : Rd.Failure : " & AL.Format(x))
    END;
    
    z := ZtraceFile.Metadata { header    := header,
                               directory := dir,
                               nsteps    := NUMBER(time^) };
    
    TRY
      ZtraceFile.Write(tWr, z)
    EXCEPT
      Wr.Failure(x) =>
      RAISE WriteError("Write error on trace file, writing header : Wr.Failure : " & AL.Format(x))
    END;

    (* write # of timesteps *)

    (* write time data *)
    VAR
      dataStartByte := Wr.Index(tWr);
      opos : CARDINAL := 0;
      pos := dataStartByte;

    BEGIN

      FOR i := 0 TO t.nNames - 1 DO
        TRY
          IF i = 0 THEN
            IF doDebug THEN
              Debug.Out(F("WriteTrace writing TIME (%s values)...",
                          Int(NUMBER(time^))))
            END;

            dir.addhi(WriteTimeTrace(tWr, time^, pos));
            <*ASSERT dir.size() = 1 *>
          ELSE
            opos := pos; (* sync pointers *)

            (* write data for node [ i ] *)
            VAR
              rep : TempDataRep.T;
            BEGIN
              t.reader.readEntireFileZ(i,
                                       rep,
                                       NUMBER(time^),
                                       targMaxDev := DefTargMaxDev);

              Debug.Out(F("WriteCompressedV1 writing data for node %s at %s",
                          Int(i),
                          Int(Wr.Index(tWr))));
              
              Wr.PutText(tWr, rep.finalData);

              pos := Wr.Index(tWr);

              VAR
                dirent : ZtraceNodeHeader.T;
              BEGIN
                (* update directory *)
                dirent.bytes := pos - opos;
                dirent.start := opos;
                dirent.norm  := rep.norm;
                dirent.code  := rep.code;
                dir.addhi(dirent);
                <*ASSERT dir.size() = i + 1*>
              END
            END
          END
        EXCEPT
          OSError.E(x) =>
          RAISE WriteError("Unable to open temp file \"" & t.fnr.name(i) & "\" for reading : OSError.E : " & AL.Format(x))
        |
          Rd.Failure(x) =>
          RAISE WriteError("Read error on temp file \"" & t.fnr.name(i) & "\" for reading : Rd.Failure : " & AL.Format(x))
        |
          Wr.Failure(x) =>
          RAISE WriteError("Write error on trace file, lately reading \"" & t.fnr.name(i) & "\" : Wr.Failure : " & AL.Format(x))
        END

      END;

      TRY
        Wr.Flush(tWr);

        (* seek back and re-write directory *)
        Debug.Out("WriteTrace re-writing directory ...");

        ZtraceFile.RewriteDirectory(tWr, z)
      EXCEPT
        Wr.Failure(x) =>
        RAISE WriteError("Write error on trace file, rewriting directory : Wr.Failure : " & AL.Format(x))
      END;

      Debug.Out("WriteCompressedV1 done:\n" & ZtraceFile.Format(z))
    END
  END WriteCompressedV1;
  
PROCEDURE WriteReordered(t : T; tWr : Wr.T)
  RAISES { WriteError } =
  (* read data from each file in temp directory and output
     in reordered trace format for fast aplot access *)
  CONST
    fmt = Version.Reordered;
  VAR
    header := Header { fmt, Time.Now(), t.nNames };

    time, data    : REF ARRAY OF LONGREAL;
    dataStartByte : CARDINAL;
  BEGIN
    IF doDebug THEN
      Debug.Out(F("WriteTrace nFiles %s names.size() %s",
                  Int(t.nFiles), Int(t.nNames)));
    
      Debug.Out("WriteTrace writing header...")
    END;
    TRY
      IF doDebug THEN
        Debug.Out("WriteTrace at tWr byte " & Int(Wr.Index(tWr)));
      END;
      
      WriteHeader(tWr, header);

      dataStartByte := Wr.Index(tWr);
    EXCEPT
      Wr.Failure(x) =>
      RAISE WriteError("Write error writing header of trace file : Wr.Failure : " &
        AL.Format(x))
    END;

    (* at this point, the header has been written *)
    
    IF doDebug THEN
      Debug.Out("WriteTrace walking names...");
    END;
    
    FOR i := 0 TO t.nNames - 1 DO
      TRY
        IF i = 0 THEN 
          IF doDebug THEN Debug.Out("WriteTrace creating buffers...") END;
          CreateBuffers(t, time, data);
          IF doDebug THEN
            Debug.Out(F("WriteTrace writing TIME (%s values)...",
                        Int(NUMBER(time^))))
          END;
          UnsafeWriter.WriteLRAAt(tWr, time^, dataStartByte);
        ELSE
          t.reader.readEntireFile(i, data^);
          
          WITH pos = dataStartByte + i * 4 * NUMBER(time^) DO
            IF doDebug THEN
              Debug.Out(F("Writing node (%s) @ %s, data[0]= %s data[LAST(data)}= %s",
                          Int(i), Int(pos),
                          LR(data[0]),
                          LR(data[LAST(data^)])))
            END;
            UnsafeWriter.WriteLRAAt(tWr, data^, pos)
          END
        END
      EXCEPT
        OSError.E(x) =>
        RAISE WriteError("Unable to open temp file \"" & t.fnr.name(i) & "\" for reading : OSError.E : " & AL.Format(x))
      |
        Rd.Failure(x) =>
        RAISE WriteError("Read error on temp file \"" & t.fnr.name(i) & "\" for reading : Rd.Failure : " & AL.Format(x))
      |
        Wr.Failure(x) =>
        RAISE WriteError("Write error on trace file, lately reading \"" & t.fnr.name(i) & "\" : Wr.Failure : " & AL.Format(x))
      END
    END;
    
  END WriteReordered;

PROCEDURE WriteHeader(wr : Wr.T; READONLY header : Header)
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    IF doDebug THEN
      Debug.Out("WriteTrace writing header...")
    END;
    UnsafeWriter.WriteI(wr, VersionVals[header.version]);
    (* this tells aplot it is the reordered format *)
    
    IF doDebug THEN
      Debug.Out("WriteTrace at wr byte " & Int(Wr.Index(wr)));
    END;
    UnsafeWriter.WriteI(wr, TRUNC(header.ctime));
    (* timestamp *)
    
    UnsafeWriter.WriteI(wr, header.nwaves);
    (* number of nodes.  
       aplot computes the offsets based on the file length? *)

    Wr.Flush(wr)
  END WriteHeader;
  
PROCEDURE ReadHeader(rd : Rd.T) : Header
  RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted, FormatError } =
  VAR
    header : Header;
  BEGIN
    VAR vval    := UnsafeReader.ReadI(rd);
        success := FALSE;
    BEGIN
      Debug.Out(F("TraceFile.ReadHeader : idx %s vval=%s",
                  Int(Rd.Index(rd) - 4), Int(vval)));
      FOR v := FIRST(VersionVals) TO LAST(VersionVals) DO
        IF vval = VersionVals[v] THEN
          header.version := v;
          success := TRUE;
          EXIT
        END
      END;
      IF NOT success THEN RAISE FormatError END
    END;
    header.ctime  := FLOAT(UnsafeReader.ReadI(rd), Time.T);
    header.nwaves := UnsafeReader.ReadI(rd);
    RETURN header
  END ReadHeader;

PROCEDURE WritePll(t                 : T;
                   wthreads          : CARDINAL;
                   writeTraceCmdPath : Pathname.T;
                   fmt               : Version) =
  (* read data from each file in temp directory and output
     in reordered trace format for fast aplot access *)
  VAR
    tFn := t.ofn & "." & VersionSuffixes[fmt];
    tWr           : Wr.T;
    time, data    : REF ARRAY OF LONGREAL;
    dataStartByte : CARDINAL;
    wcs := NEW(REF ARRAY OF WriteClosure, wthreads);
    
  BEGIN

    IF fmt # Version.Reordered THEN
      Debug.Error("Parallel write only supported for -format Reordered")
    END;
    
    Debug.Out(F("WriteTracePll nFiles %s t.nNames %s",
                Int(t.nFiles), Int(t.nNames)));
    
    TRY
      tWr := FileWr.Open(tFn)
    EXCEPT
      OSError.E(x) => Debug.Error("Unable to open trace file \"" & tFn & "\" for writing : OSError.E : " & AL.Format(x))
    END;

    TRY
      WriteHeader(tWr, Header { Version.Reordered, Time.Now(), t.nNames });
    
      dataStartByte := Wr.Index(tWr);
    EXCEPT
      Wr.Failure(x) =>
      Debug.Error("Write error writing header of trace file : Wr.Failure : " & AL.Format(x))
    END;

    IF doDebug THEN
      Debug.Out("WriteTrace walking names...");
    END;

    WITH i = 0 DO
      TRY
        IF doDebug THEN Debug.Out("WriteTrace creating buffers...") END;
        CreateBuffers(t, time, data);
        IF doDebug THEN
          Debug.Out(F("WriteTrace writing TIME (%s values) at %s...",
                      Int(NUMBER(time^)),
                      Int(dataStartByte)))
        END;
        UnsafeWriter.WriteLRAAt(tWr, time^, dataStartByte);

        Wr.Close(tWr)
      EXCEPT
        OSError.E(x) =>
        Debug.Error("Unable to open temp file \"" & t.fnr.name(i) & "\" for reading : OSError.END : " & AL.Format(x))
      |
        Rd.Failure(x) =>
        Debug.Error("Read error on temp file \"" & t.fnr.name(i) & "\" for reading : Rd.Failure : " & AL.Format(x))
      |
        Wr.Failure(x) =>
        Debug.Error("Write error on trace file, lately reading \"" & t.fnr.name(i) & "\" : Wr.Failure : " & AL.Format(x))
      END
    END;

    (* zero fill file if it contains any data *)
    IF t.nNames # 0 AND NUMBER(time^) # 0 THEN
      TRY
        WITH file = NARROW(FS.OpenFile(tFn, truncate := FALSE), RegularFile.T) DO
          EVAL file.seek(RegularFile.Origin.Beginning,
                         dataStartByte + t.nNames * 4 * NUMBER(time^) - 1);
          file.write(ARRAY OF File.Byte { ORD('*') }); (* EOF sentinel *)
          file.close();
        END
      EXCEPT
        OSError.E(x) =>
        Debug.Error("Unable to zero-fill output trace file \""& tFn & "\" for reading : OSError.END : " & AL.Format(x))
      END
    END;

    (* the range is 
       
       from 1 to t.nNames - 1 
       
    *)

    WITH c      = NEW(Thread.Condition),
         mu     = NEW(MUTEX),
         nNodes = t.nNames - 1,
         per = (nNodes - 1) DIV wthreads + 1 DO

      Debug.Out(F("Spawning write %s threads, %s nodes per thread",
                  Int(wthreads), Int(per)));
      
      FOR i := FIRST(wcs^) TO LAST(wcs^) DO
        WITH lo = 1 + per * i,
             hi = MIN(1 + per * (i + 1), t.nNames - 1) DO

          IF writeTraceCmdPath = NIL THEN
            TRY
              WITH wr = NEW(FileWr.T).init(FS.OpenFile(tFn, truncate := FALSE)) DO             
                wcs[i] := NEW(IntWriteClosure).init(t.fnr,
                                                    wr,
                                                    c,
                                                    mu,
                                                    lo, hi,
                                                    dataStartByte,
                                                    NUMBER(time^))
              END
            EXCEPT
              OSError.E(x) =>
              Debug.Error(F("Unable to re-open output trace file \"%s\" [i=%s lo=%s hi=%s] for reading : OSError.END : %s", tFn, Int(lo), Int(hi), AL.Format(x)))
            END   
          ELSE
            wcs[i] := NEW(ExtWriteClosure).init(c, mu,
                                                lo, hi,
                                                dataStartByte,
                                                NUMBER(time^),
                                                writeTraceCmdPath,
                                                t.fnr.getWd(),
                                                t.nFiles,
                                                t.nNames,
                                                tFn)
          END
            
        END
      END;

      (* wait for threads to exit *)
      
      Debug.Out("Waiting for write threads to finish...");
      FOR i := FIRST(wcs^) TO LAST(wcs^) DO
        WHILE NOT wcs[i].doneP() DO
          LOCK mu DO Thread.Wait(mu, c) END;
        END;
(*        EVAL Thread.Join(wcs[i].thr)*)
      END;

      Debug.Out("Write threads have finished.");

    END;
  END WritePll;

  (**********************************************************************)

TYPE
  WriteClosure = Thread.Closure OBJECT
    lo, hi        : CARDINAL;
    samples       : CARDINAL;
    mu            : MUTEX;
    c             : Thread.Condition;
    dataStartByte : CARDINAL;
    thr           : Thread.T;
    
    done := FALSE;
  METHODS
    doneP() : BOOLEAN := WCDoneP;
    
  END;

  IntWriteClosure = WriteClosure OBJECT (* internal write *)
    fnr    : FileNamer.T;
    wr     : Wr.T;             
  METHODS
    init(fnr           : FileNamer.T;
         wr            : Wr.T;
         c             : Thread.Condition;
         mu            : MUTEX;
         lo, hi        : CARDINAL;
         dataStartByte : CARDINAL;
         samples       : CARDINAL) : WriteClosure := WCInit;
  OVERRIDES
    apply := WCApply;
  END;

  ExtWriteClosure = WriteClosure OBJECT (* external write *)
    wd     : Pathname.T;
    nFiles : CARDINAL;
    nNames : CARDINAL;
    tFn    : Pathname.T;
    wPath  : Pathname.T;
  METHODS
    init(c : Thread.Condition;
         mu : MUTEX;
         lo, hi : CARDINAL;
         dataStartByte : CARDINAL;
         samples : CARDINAL;
         wPath : Pathname.T;
         wd : Pathname.T;
         nFiles, nNames : CARDINAL;
         tFn : Pathname.T) : WriteClosure :=
    XWCInit;
  OVERRIDES
    apply := XWCApply;
  END;

PROCEDURE XWCInit(xwc : ExtWriteClosure;

                  (* shared args *)
                  c : Thread.Condition;
                  mu : MUTEX;
                  lo, hi : CARDINAL;
                  dataStartByte : CARDINAL;
                  samples : CARDINAL;

                  (* special args *)
                  wPath : Pathname.T;
                  wd : Pathname.T;
                  nFiles, nNames : CARDINAL;
                  tFn : Pathname.T) : WriteClosure =
  BEGIN
    xwc.mu := mu;
    xwc.c := c;
    xwc.samples := samples;
    xwc.dataStartByte := dataStartByte;
    xwc.lo := lo;
    xwc.hi := hi;

    xwc.wd := wd;
    xwc.nFiles := nFiles;
    xwc.nNames := nNames;
    xwc.tFn := tFn;
    xwc.wPath := wPath;
    xwc.thr := Thread.Fork(xwc);
    
    RETURN xwc
  END XWCInit;

PROCEDURE XWCApply(xwc : ExtWriteClosure) : REFANY =
  TYPE
    TA = ARRAY OF TEXT;
  BEGIN
    WITH cmd = FN("%s %s %s %s %s %s %s %s %s",
                  TA { xwc.wPath,
                       xwc.wd,
                       Int(xwc.dataStartByte),
                       Int(xwc.samples),
                       Int(xwc.nFiles),
                       Int(xwc.nNames),
                       Int(xwc.lo),
                       Int(xwc.hi),
                       xwc.tFn }) DO

      Debug.Out(F("TraceFile.XWCApply: Running command \"%s\"", cmd));
      WITH
        completion = ProcUtils.RunText(cmd, NIL, NIL, NIL) DO
        TRY
          completion.wait()
        EXCEPT
          ProcUtils.ErrorExit(err) =>
          Debug.Out(F("Command \"%s\" failed : ProcUtils.ErrorExit : %s",
                      cmd, ProcUtils.FormatError(err)))
        END
      END
    END;
    
    LOCK xwc.mu DO
      (* done executing request, mark ourselves as free and signal *)
      xwc.done := TRUE;
    END;
    
    Thread.Signal(xwc.c);

    RETURN NIL
    
  END XWCApply;

PROCEDURE WCDoneP(cl : WriteClosure) : BOOLEAN =
  BEGIN
    LOCK cl.mu DO
      RETURN cl.done
    END
  END WCDoneP;

PROCEDURE WCInit(cl            : IntWriteClosure;
                 fnr           : FileNamer.T;
                 wr            : Wr.T;
                 c             : Thread.Condition;
                 mu            : MUTEX;
                 lo, hi        : CARDINAL;
                 dataStartByte : CARDINAL;
                 samples       : CARDINAL) : WriteClosure =
  BEGIN
    cl.mu := mu;
    cl.c := c;
    cl.wr := wr;
    cl.samples := samples;
    cl.dataStartByte := dataStartByte;
    cl.lo := lo;
    cl.hi := hi;

    cl.fnr := fnr;
    cl.thr := Thread.Fork(cl);
    RETURN cl
  END WCInit;

PROCEDURE WCApply(cl : IntWriteClosure) : REFANY =
  VAR
    buff := NEW(REF ARRAY OF LONGREAL, cl.samples);
    fr   := NEW(TempReader.T).init(cl.fnr);
    idx  : CARDINAL; (* for error messages *)
  BEGIN
    TRY
      FOR i := cl.lo TO cl.hi DO
        idx := i;
        BlockWrite(cl.wr, fr, i, cl.dataStartByte, buff^)
      END;

      TRY
        Wr.Close(cl.wr)
      EXCEPT
        Wr.Failure(x) => Debug.Error(F("Trouble closing trace file for worker %s: Wr.Failure : %s", Int(idx),
                                       AL.Format(x)))
      END;
      
      LOCK cl.mu DO
        (* done executing request, mark ourselves as free and signal *)
        cl.done := TRUE;
      END;
        
      Thread.Signal(cl.c)
        
    EXCEPT
      OSError.E(x) =>
        Debug.Error("Unable to open temp file \"" & cl.fnr.name(idx) & "\" for reading : OSError.E : " & AL.Format(x))
      |
        Rd.Failure(x) =>
        Debug.Error("Read error on temp file \"" & cl.fnr.name(idx) & "\" for reading : Rd.Failure : " & AL.Format(x))
      |
        Wr.Failure(x) =>
        Debug.Error("Write error on trace file, lately reading \"" & cl.fnr.name(idx) & "\" : Wr.Failure : " & AL.Format(x))
    END;
    
    RETURN NIL
  END WCApply;

  (***********************************************************************)

PROCEDURE BlockWrite(wr            : Wr.T;
                     fr            : TempReader.T;
                     i             : CARDINAL;
                     dataStartByte : CARDINAL;
                     VAR buff      : ARRAY OF LONGREAL)
  RAISES { Rd.Failure, Wr.Failure, OSError.E, Thread.Alerted } =
  BEGIN
    (* 
       XXX fr (somehow) will return 
            a Compressed format 
                   OR 
            a LRA buffer 
    *)

    (* 
       if it is a compressed format, we can either 
       (1) uncompress here and write to the target
       (2) if allowed, pass through to a compressed trace format (TBD!)
    *)
           
    fr.readEntireFile(i, buff);
        
    WITH pos = dataStartByte + i * 4 * NUMBER(buff) DO
      IF doDebug THEN
        Debug.Out(F("Writing node(%s) @ %s, data[0]= %s data[LAST(data)]= %s",
                    Int(i), Int(pos),
                    LR(buff[0]),
                    LR(buff[LAST(buff)])))
      END;
      UnsafeWriter.WriteLRAAt(wr, buff, pos)
    END;
  END BlockWrite;

PROCEDURE FormatHeader(READONLY h : Header) : TEXT =
  BEGIN
    RETURN F("{ version %s(%s) ctime %s nwaves %s }",
             VersionNames[h.version], Int(VersionVals[h.version]),
             LR(h.ctime), Int(h.nwaves))
  END FormatHeader;

BEGIN END TraceFile.
