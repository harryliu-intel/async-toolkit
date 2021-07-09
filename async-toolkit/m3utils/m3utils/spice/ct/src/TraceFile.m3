MODULE TraceFile;
IMPORT Wr;
IMPORT Debug;
FROM Fmt IMPORT Int, F, LongReal;
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

<*FATAL Thread.Alerted*>

CONST LR = LongReal;

VAR doDebug := Debug.DebugThis("TraceFile");

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
    init := Init;
    write := Write;
    writePll := WritePll;
  END;

PROCEDURE Init(t      : T;
               ofn    : Pathname.T;
               nFiles : CARDINAL;
               nNames : CARDINAL;
               fnr    : FileNamer.T) : T =
  BEGIN
    t.ofn := ofn;
    t.nFiles := nFiles;
    t.nNames := nNames;
    t.fnr := fnr;
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

PROCEDURE Write(t : T) =
  (* read data from each file in temp directory and output
     in reordered trace format for fast aplot access *)
  VAR
    tFn := t.ofn & ".trace";
    tWr           : Wr.T;
    time, data    : REF ARRAY OF LONGREAL;
    dataStartByte : CARDINAL;
  BEGIN
    Debug.Out(F("WriteTrace nFiles %s names.size() %s",
                Int(t.nFiles), Int(t.nNames)));
    
    TRY
      tWr := FileWr.Open(tFn)
    EXCEPT
      OSError.E(x) => Debug.Error("Unable to open trace file \"" & tFn & "\" for writing : OSError.E : " & AL.Format(x))
    END;

    IF doDebug THEN
      Debug.Out("WriteTrace writing header...")
    END;
    TRY
      IF doDebug THEN
        Debug.Out("WriteTrace at tWr byte " & Int(Wr.Index(tWr)));
      END;
      UnsafeWriter.WriteI(tWr, 1);
      (* this tells aplot it is the reordered format *)

      IF doDebug THEN
        Debug.Out("WriteTrace at tWr byte " & Int(Wr.Index(tWr)));
      END;
      UnsafeWriter.WriteI(tWr, TRUNC(Time.Now()));
      (* timestamp *)
      
      UnsafeWriter.WriteI(tWr, t.nNames);
      (* number of nodes.  
         aplot computes the offsets based on the file length? *)

      dataStartByte := Wr.Index(tWr);
    EXCEPT
      Wr.Failure(x) =>
      Debug.Error("Write error writing header of trace file : Wr.Failure : " & AL.Format(x))
    END;

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
        Debug.Error("Unable to open temp file \"" & t.fnr.name(i) & "\" for reading : OSError.E : " & AL.Format(x))
      |
        Rd.Failure(x) =>
        Debug.Error("Read error on temp file \"" & t.fnr.name(i) & "\" for reading : Rd.Failure : " & AL.Format(x))
      |
        Wr.Failure(x) =>
        Debug.Error("Write error on trace file, lately reading \"" & t.fnr.name(i) & "\" : Wr.Failure : " & AL.Format(x))
      END
    END;

    TRY
      Wr.Close(tWr)
    EXCEPT
      Wr.Failure(x) => Debug.Error("Trouble closing trace file : Wr.Failure : " &
        AL.Format(x))
    END
  END Write;

PROCEDURE WritePll(t : T; wthreads : CARDINAL) =
  (* read data from each file in temp directory and output
     in reordered trace format for fast aplot access *)
  VAR
    tFn := t.ofn & ".trace";
    tWr           : Wr.T;
    time, data    : REF ARRAY OF LONGREAL;
    dataStartByte : CARDINAL;
    wcs := NEW(REF ARRAY OF WriteClosure, wthreads);
    
  BEGIN
    Debug.Out(F("WriteTracePll nFiles %s t.nNames %s",
                Int(t.nFiles), Int(t.nNames)));
    
    TRY
      tWr := FileWr.Open(tFn)
    EXCEPT
      OSError.E(x) => Debug.Error("Unable to open trace file \"" & tFn & "\" for writing : OSError.E : " & AL.Format(x))
    END;

    IF doDebug THEN
      Debug.Out("WriteTrace writing header...")
    END;
    TRY
      IF doDebug THEN
        Debug.Out("WriteTrace at tWr byte " & Int(Wr.Index(tWr)));
      END;
      UnsafeWriter.WriteI(tWr, 1);
      (* this tells aplot it is the reordered format *)

      IF doDebug THEN
        Debug.Out("WriteTrace at tWr byte " & Int(Wr.Index(tWr)));
      END;
      UnsafeWriter.WriteI(tWr, TRUNC(Time.Now()));
      (* timestamp *)
      
      UnsafeWriter.WriteI(tWr, t.nNames);
      (* number of nodes.  
         aplot computes the offsets based on the file length? *)

      Wr.Flush(tWr);
      
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
          Debug.Out(F("WriteTrace writing TIME (%s values)...",
                      Int(NUMBER(time^))))
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

    WITH c = NEW(Thread.Condition),
         mu = NEW(MUTEX),
         nNodes = t.nNames - 1,
         per = (nNodes - 1) DIV wthreads + 1 DO

      Debug.Out(F("Spawning write %s threads, %s nodes per thread",
                  Int(wthreads), Int(per)));
      
      FOR i := FIRST(wcs^) TO LAST(wcs^) DO
        WITH wr = NEW(FileWr.T).init(FS.OpenFile(tFn, truncate := FALSE)),
             lo = 1 + per * i,
             hi = MIN(1 + per * (i + 1), t.nNames - 1) DO
          
          wcs[i] := NEW(WriteClosure).init(t.fnr,
                                           wr,
                                           c,
                                           mu,
                                           lo, hi,
                                           dataStartByte,
                                           NUMBER(time^))
          
        END
      END;

      (* wait for threads to exit *)
      
      Debug.Out("Waiting for write threads to finish...");
      FOR i := FIRST(wcs^) TO LAST(wcs^) DO
        WHILE NOT wcs[i].doneP() DO
          LOCK mu DO Thread.Wait(mu, c) END;
        END;
(*        EVAL Thread.Join(wcs[i].thr)*)
        TRY
          Wr.Close(wcs[i].wr)
        EXCEPT
          Wr.Failure(x) => Debug.Error(F("Trouble closing trace file for worker %s: Wr.Failure : %s", Int(i),
            AL.Format(x)))
        END
      END;

      Debug.Out("Write threads have finished.");

    END;
  END WritePll;

  (**********************************************************************)

TYPE
  WriteClosure = Thread.Closure OBJECT
    fnr : FileNamer.T;
    wr     : Wr.T;             
    lo, hi : CARDINAL;
    samples : CARDINAL;
    mu : MUTEX;
    c : Thread.Condition;
    done := FALSE;
    dataStartByte : CARDINAL;
    thr : Thread.T;
  METHODS
    init(fnr : FileNamer.T;
         wr : Wr.T;
         c : Thread.Condition;
         mu : MUTEX;
         lo, hi : CARDINAL;
         dataStartByte : CARDINAL;
         samples : CARDINAL) : WriteClosure := WCInit;

    doneP() : BOOLEAN := WCDoneP;
    
  OVERRIDES
    apply := WCApply;
  END;


PROCEDURE WCDoneP(cl : WriteClosure) : BOOLEAN =
  BEGIN
    LOCK cl.mu DO
      RETURN cl.done
    END
  END WCDoneP;

PROCEDURE WCInit(cl : WriteClosure;
                 fnr : FileNamer.T;
                 wr : Wr.T;
                 c : Thread.Condition;
                 mu : MUTEX;
                 lo, hi : CARDINAL;
                 dataStartByte : CARDINAL;
                 samples : CARDINAL) : WriteClosure =
  BEGIN
    cl.fnr := fnr;
    cl.mu := mu;
    cl.c := c;
    cl.wr := wr;
    cl.samples := samples;
    cl.dataStartByte := dataStartByte;
    cl.lo := lo;
    cl.hi := hi;
    cl.thr := Thread.Fork(cl);
    RETURN cl
  END WCInit;

PROCEDURE WCApply(cl : WriteClosure) : REFANY =
  VAR
    buff := NEW(REF ARRAY OF LONGREAL, cl.samples);
    fr := NEW(TempReader.T).init(cl.fnr);
    idx : CARDINAL; (* for error messages *)
  BEGIN
    TRY
      FOR i := cl.lo TO cl.hi DO
        idx := i; 
        fr.readEntireFile(i, buff^);
        
        WITH pos = cl.dataStartByte + i * 4 * cl.samples DO
          IF doDebug THEN
            Debug.Out(F("Writing node(%s) @ %s, data[0]= %s data[LAST(data)]= %s",
                        Int(i), Int(pos),
                        LR(buff[0]),
                        LR(buff[LAST(buff^)])))
          END;
          UnsafeWriter.WriteLRAAt(cl.wr, buff^, pos)
        END;
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

BEGIN END TraceFile.
