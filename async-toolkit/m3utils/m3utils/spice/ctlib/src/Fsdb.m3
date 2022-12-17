MODULE Fsdb;

IMPORT TextSeq;
IMPORT TextSet;
IMPORT RegExList;
IMPORT Pathname;
IMPORT Rd;
IMPORT Wr;
IMPORT ProcUtils;
IMPORT Debug;
FROM Fmt IMPORT F, Int, LongReal;
IMPORT TextReader;
IMPORT Text;
IMPORT CitTextUtils AS TextUtils, Lex, FloatMode;
IMPORT Scan;
IMPORT LongRealSeq AS LRSeq;
IMPORT Word;
IMPORT NameControl;
IMPORT CardSeq;
IMPORT OSError;
IMPORT Thread;
IMPORT AL;
FROM Tr0 IMPORT RenameBack;
IMPORT UnsafeReader;
IMPORT UnsafeWriter;
IMPORT TextSetDef;
IMPORT DataBlock;
IMPORT TextCardTbl;
IMPORT TextCardSeqTbl;
IMPORT SpiceCompress;
IMPORT TextWr;

<*FATAL Thread.Alerted*>

CONST TE = Text.Equal;
      LR = LongReal;

VAR doDebug := Debug.DebugThis("Fsdb");

PROCEDURE EditName(nm : TEXT) : TEXT =
  CONST
    RemoveVoltPrefix = TRUE;
  BEGIN
    IF RemoveVoltPrefix
      AND (TextUtils.HavePrefix(nm, "v(") OR TextUtils.HavePrefix(nm, "V("))
     THEN
      RETURN TextUtils.RemoveSuffixes(Text.Sub(nm, 2), ARRAY OF TEXT { ")" })
    ELSE
      RETURN nm
    END
  END EditName;

  (**********************************************************************)

  (* the following procedures communicate with a nanosimrd process *)
      
PROCEDURE PutCommandG(wr : Wr.T; cmd : TEXT) =
  BEGIN
    TRY
      IF doDebug THEN
        Debug.Out(F("Fsdb.Parse.PutCommand \"%s\"", cmd));
      END;
      Wr.PutText(wr, cmd);
      Wr.PutChar(wr, '\n');
      Wr.Flush(wr);
    EXCEPT
      Wr.Failure(x) =>
      Debug.Error("Unexpected Wr.Failure in PutCommand : " & AL.Format(x))
    END
  END PutCommandG;

PROCEDURE GetResponseG(rd : Rd.T; matchKw : TEXT) : TextReader.T =
  VAR
    kw : TEXT;
  BEGIN
    TRY
      LOOP
        WITH line    = Rd.GetLine(rd),
             reader  = NEW(TextReader.T).init(line) DO
          IF doDebug THEN
            Debug.Out(F("Fsdb.Parse.GetResponse \"%s\"", line));
          END;
          IF reader.next(" ", kw, TRUE) THEN
            IF TE(kw, matchKw) THEN
              RETURN reader
            END
          END
        END
      END
    EXCEPT
      Rd.Failure(x) =>
      Debug.Error("Unexpected Rd.Failure in GetResponse : " & AL.Format(x)); 
      <*ASSERT FALSE*>
      
    |
      Rd.EndOfFile =>
      Debug.Error("Unexpected Rd.EndOfFile in GetResponse");
      <*ASSERT FALSE*>
    END
  END GetResponseG;

PROCEDURE ReadCompressedNodeDataG(rd         : Rd.T;
                                  VAR nodeid : CARDINAL;
                                  VAR norm   : SpiceCompress.Norm) : TEXT =
  VAR
    kw    : TEXT;
    bytes : CARDINAL;
    tag   : CHAR;
  BEGIN
    TRY
      LOOP
        WITH line    = Rd.GetLine(rd),
             reader  = NEW(TextReader.T).init(line) DO
          IF doDebug THEN
            Debug.Out(F("Fsdb.Parse.ReadCompressedNodeDataG line \"%s\"", line))
          END;

          IF reader.next(" ", kw, TRUE) THEN
            IF    TE(kw, "E") THEN
              Debug.Error(F("Got time mismatch: nodeid %s: line %s",
                            Int(nodeid), line));
              <*ASSERT FALSE*>
            ELSIF TE(kw, "ZZZ") THEN
              (* this code synchronized with Main.m3<spicestream> *)
              tag      := Rd.GetChar(rd);
              <*ASSERT tag = 'x'*>
              nodeid   := UnsafeReader.ReadI(rd);
              bytes    := UnsafeReader.ReadI(rd);
              norm.min := UnsafeReader.ReadLR(rd);
              norm.max := UnsafeReader.ReadLR(rd);

              Debug.Out(F("Fsdb.ReadCompressedNodeDataG got nodeid %s bytes %s m in %s max %s",
                          Int(nodeid), Int(bytes), LR(norm.min), LR(norm.max) ));
              
              WITH buflen  = bytes - 8, (* 9 is len of min, max, code *)
                   chars   = NEW(REF ARRAY OF CHAR, buflen),
                   got     = Rd.GetSub(rd, chars^) DO
                IF got # buflen THEN RAISE Rd.EndOfFile END;

                IF doDebug THEN
                  Debug.Out(F("ReadCompressedNodeDataG nodeid %s bytes %s min %s max %s",
                              Int(nodeid), Int(bytes),
                              LR(norm.min), LR(norm.max)))
                END;
                RETURN Text.FromChars(chars^)
              END
            ELSE
              Debug.Error(F("?syntax error : ReadBinaryNodeData: got \"%s\"",
                            line));
              <*ASSERT FALSE*>
            END
          END
        END
      END
    EXCEPT
      Rd.Failure(x) =>
      Debug.Error("Unexpected Rd.Failure in ReadCompressedNodeData : " &
        AL.Format(x));
      <*ASSERT FALSE*>
    |
      Rd.EndOfFile =>
      Debug.Error("Unexpected Rd.EndOfFile in ReadCompressedNodeData");
      <*ASSERT FALSE*>
    END
  END ReadCompressedNodeDataG;
  
PROCEDURE ReadBinaryNodeDataG(rd         : Rd.T;
                              VAR nodeid : CARDINAL;
                              VAR buff   : ARRAY OF LONGREAL) =
  VAR
    kw : TEXT;
    n  : CARDINAL;
  BEGIN
    TRY
      LOOP
        WITH line    = Rd.GetLine(rd),
             reader  = NEW(TextReader.T).init(line) DO
          IF doDebug THEN
            Debug.Out(F("Fsdb.Parse.ReadBinaryNodeData line \"%s\"", line))
          END;

          IF reader.next(" ", kw, TRUE) THEN
            IF    TE(kw, "E") THEN
              Debug.Error(F("Got time mismatch: nodeid %s: line %s",
                            Int(nodeid), line))
            ELSIF TE(kw, "OK") THEN
              WITH tag = Rd.GetChar(rd) DO
                nodeid := UnsafeReader.ReadI(rd);
                n      := UnsafeReader.ReadI(rd);

                IF doDebug THEN
                  Debug.Out(F("ReadBinaryNodeData tag %s nodeid %s n %s",
                              Text.FromChar(tag),
                              Int(nodeid),
                              Int(n)))
                END;
                
                IF n # NUMBER(buff) THEN
                  Debug.Error(F("Size mismatch n %s # NUMBER(buff) %s",
                                Int(n), Int(NUMBER(buff))))
                END;
                UnsafeReader.ReadLRA(rd, buff);

                IF doDebug THEN
                  Debug.Out(F("ReadBinaryNodeData buff[0] %s",
                              LR(buff[0])))
                END;
                RETURN
                END
            ELSE
              Debug.Error(F("?syntax error : ReadBinaryNodeData: got \"%s\"",
                            line))
            END
          END
        END
      END
    EXCEPT
      Rd.Failure(x) =>
      Debug.Error("Unexpected Rd.Failure in ReadBinaryNodeData : " & AL.Format(x));
      <*ASSERT FALSE*>
    |
      Rd.EndOfFile =>
      Debug.Error("Unexpected Rd.EndOfFile in ReadBinaryNodeData");
      <*ASSERT FALSE*>
    END
  END ReadBinaryNodeDataG;

PROCEDURE ReadInterpolatedBinaryNodeDataG(rd          : Rd.T;
                                          VAR nodeid  : CARDINAL;
                                          VAR buff    : ARRAY OF LONGREAL;
                                          interpolate : LONGREAL;
                                          unit        : LONGREAL) =
  VAR
    kw : TEXT;
    n  : CARDINAL;
  BEGIN
    <*ASSERT interpolate # 0.0d0*>
    TRY
      LOOP
        WITH line    = Rd.GetLine(rd),
             reader  = NEW(TextReader.T).init(line) DO
          IF doDebug THEN
            Debug.Out(F("Fsdb.Parse.ReadInterpolatedBinaryNodeData line \"%s\"", line))
          END;

          IF reader.next(" ", kw, TRUE) THEN
            IF    TE(kw, "E") THEN
              Debug.Error(F("Got time mismatch: nodeid %s: line %s",
                            Int(nodeid), line))
            ELSIF TE(kw, "OK") THEN
              WITH tag = Rd.GetChar(rd) DO
                nodeid := UnsafeReader.ReadI(rd);
                n      := UnsafeReader.ReadI(rd);

                IF doDebug THEN
                  Debug.Out(F("ReadInterpolatedBinaryNodeData tag %s nodeid %s n %s",
                              Text.FromChar(tag),
                              Int(nodeid),
                              Int(n)))
                END;

                WITH hi = NEW(REF ARRAY OF CARDINAL, n),
                     lo = NEW(REF ARRAY OF CARDINAL, n),
                     vv = NEW(REF ARRAY OF LONGREAL, n) DO
                  UnsafeReader.ReadUA (rd, hi^);
                  UnsafeReader.ReadUA (rd, lo^);
                  UnsafeReader.ReadLRA(rd, vv^);
                  
                  IF n = 0 AND NUMBER(buff) # 0 THEN
                    Debug.Error("?no data")
                  END;
                  
                  VAR
                    j := 0;
                    y := vv[j];
                    py := y;
                    dt := 0.0d0;
                    pt := 0.0d0; 
                  BEGIN
                    FOR i := FIRST(buff) TO LAST(buff) DO
                      WITH st = FLOAT(i, LONGREAL) * interpolate DO
                        WHILE st > dt DO
                          INC(j);
                          IF j > LAST(vv^) THEN
                            pt := dt;
                            dt := LAST(LONGREAL)
                          ELSE
                            pt := dt;
                            dt := (FLOAT(hi[j],LONGREAL) * TwoToThe32 + FLOAT(lo[j], LONGREAL)) * unit;
                            py := y;
                            y := vv[j]
                          END
                        END;
                        
                        <*ASSERT st <= dt*>
                        
                        IF    dt = LAST(LONGREAL) OR dt = st THEN
                          (* fill with the last data point *)
                          buff[i] := y
                        ELSE
                          <*ASSERT dt # pt*>
                          (* normal interpolation case *)
                          buff[i] := (st - pt) / (dt - pt) * (y - py) + py
                        END
                      END
                    END
                  END;
                  
                  IF doDebug THEN
                    Debug.Out(F("ReadInterpolatedBinaryNodeData buff[0] %s buff[LAST(buff)] %s",
                                LR(buff[0]), LR(buff[LAST(buff)])))
                  END;
                  RETURN
                END
              END
            ELSE
              Debug.Error(F("?syntax error : ReadInterpolatedBinaryNodeData: got \"%s\"",
                            line))
            END
          END
        END
      END
    EXCEPT
      Rd.Failure(x) =>
      Debug.Error("Unexpected Rd.Failure in ReadInterpolatedBinaryNodeData : " & AL.Format(x));
      <*ASSERT FALSE*>
    |
      Rd.EndOfFile =>
      Debug.Error("Unexpected Rd.EndOfFile in ReadInterpolatedBinaryNodeData");
      <*ASSERT FALSE*>
    END
  END ReadInterpolatedBinaryNodeDataG;

PROCEDURE GetLineUntilG(rd : Rd.T; term : TEXT; VAR line : TEXT) : BOOLEAN =
  BEGIN
    TRY
      WITH this    = Rd.GetLine(rd) DO
        IF TE(this, term) THEN
          RETURN FALSE
        ELSE
          line := this;
          RETURN TRUE
        END
      END
    EXCEPT
      Rd.Failure(x) =>
      Debug.Error("Unexpected Rd.Failure in GetLineUntil : " & AL.Format(x));
      <*ASSERT FALSE*>
    |
      Rd.EndOfFile =>
      Debug.Error("Unexpected Rd.EndOfFile in GetLineUntil");
      <*ASSERT FALSE*>
    END
  END GetLineUntilG;

  (**********************************************************************)
  
CONST
  TwoToThe32 = FLOAT(Word.Shift(1, 32), LONGREAL);

PROCEDURE StoreByType(tbl : TextCardSeqTbl.T; type : TEXT; fsdbId : CARDINAL) =
  VAR
    seq : CardSeq.T;
  BEGIN
    IF NOT tbl.get(type, seq) THEN
      seq := NEW(CardSeq.T).init();
      EVAL tbl.put(type, seq)
    END;
    seq.addhi(fsdbId)
  END StoreByType;

PROCEDURE GetIdsByType(tbl : TextCardSeqTbl.T; type : TEXT) : CardSeq.T =
  VAR
    seq : CardSeq.T;
  BEGIN
    IF NOT tbl.get(type, seq) THEN
      seq := NEW(CardSeq.T).init();
      EVAL tbl.put(type, seq)
    END;
    RETURN seq
  END GetIdsByType;

PROCEDURE InterpolateTimesteps(VAR timesteps : LRSeq.T; interpolate : LONGREAL) =
  BEGIN
    (* rewrite all timesteps based on interpolation *)
    WITH lo  = timesteps.get(0),
         hi  = timesteps.get(timesteps.size() - 1),
         cnt = (hi - lo) / interpolate,
         n   = TRUNC(cnt) DO

      timesteps := timesteps.init();

      FOR i := 0 TO n - 1 DO
        WITH t = interpolate * FLOAT(i, LONGREAL) DO
          timesteps.addhi(t)
        END
      END;
      
      IF doDebug THEN
        Debug.Out(F("interpolating : hi / interpolate = %s, n = %s : min %s max %s",
                    LR(cnt), Int(n),
                    LR(timesteps.get(0)), LR(timesteps.get(timesteps.size()-1))))
      END
      
    END
  END InterpolateTimesteps;

PROCEDURE WriteTimesteps(READONLY wdWr : ARRAY OF Wr.T;
                         timesteps : LRSeq.T;
                         timeScaleFactor, timeOffset : LONGREAL;
                         nFiles : CARDINAL;
                         ) =
  VAR
    arr := NEW(REF ARRAY OF LONGREAL, timesteps.size());
  BEGIN
    IF doDebug THEN
      Debug.Out(F("Writing timesteps, steps %s", Int(timesteps.size())));
    END;
    FOR i := 0 TO timesteps.size() - 1 DO
      arr[i] := timeScaleFactor * timesteps.get(i) + timeOffset
    END;
    WITH fIdx = NameControl.FileIndex(nFiles, 0, 0) DO
      <*ASSERT wdWr[fIdx] # NIL*>
      
      TRY
        DataBlock.WriteData(wdWr[fIdx], 0, arr^)
      EXCEPT
        Wr.Failure(x) =>
        Debug.Error("Wr.Failure writing time steps : " & AL.Format(x))
      END
    END
  END WriteTimesteps;

PROCEDURE GenSingleThreaded(rd                  : Rd.T;
                            wr                  : Wr.T;
                            idxMap              : CardSeq.T;
                            timesteps           : LRSeq.T;
                            READONLY fileTab    : ARRAY OF CardSeq.T;
                            READONLY wdWr       : ARRAY OF Wr.T;
                            READONLY wdPth      : ARRAY OF Pathname.T;
                            compressPath        : Pathname.T;
                            compressPrec        : LONGREAL;
                            voltageScaleFactor,
                            voltageOffset,
                            interpolate,
                            unit                : LONGREAL) =
  BEGIN
    FOR i := FIRST(fileTab) TO LAST(fileTab) DO
      IF doDebug THEN
        Debug.Out(F("Fsdb.Parse : Generating partial trace file %s",
                    Int(i)));
      END;
      
      GeneratePartialTraceFile(wdWr[i],
                               fileTab[i],
                               idxMap,
                               rd,
                               wr, 
                               timesteps.size(),
                               compressPath,
                               compressPrec,
                               voltageScaleFactor,
                               voltageOffset,
                               interpolate,
                               unit,
                               wdPth[i])
    END
  END GenSingleThreaded;
  
PROCEDURE GenMultiThreaded(threads               : CARDINAL;
                           idxMap                : CardSeq.T;
                           timesteps             : LRSeq.T;
                           READONLY fileTab      : ARRAY OF CardSeq.T;
                           READONLY wdWr         : ARRAY OF Wr.T;
                           READONLY wdPth        : ARRAY OF Pathname.T;
                           cmdPath, fsdbPath     : Pathname.T;
                           compressPath          : Pathname.T;
                           compressPrec          : LONGREAL;
                           voltageScaleFactor,
                           voltageOffset,
                           interpolate,
                           unit                  : LONGREAL) =
  VAR
    workers     := NEW(REF ARRAY OF GenClosure, threads);
    c, d        := NEW(Thread.Condition);
    mu          := NEW(MUTEX);
    assigned : BOOLEAN;
  BEGIN
    FOR w := FIRST(workers^) TO LAST(workers^) DO
      (* start workers *)
      workers[w] := NEW(GenClosure).init(c,
                                         d,
                                         mu,
                                         idxMap,
                                         timesteps.size(),
                                         cmdPath,
                                         fsdbPath,
                                         compressPath,
                                         compressPrec,
                                         voltageScaleFactor,
                                         voltageOffset,
                                         interpolate,
                                         unit)
    END;
    
    FOR i := FIRST(fileTab) TO LAST(fileTab) DO
      IF doDebug THEN
        Debug.Out(F("Fsdb.Parse : Generating partial trace file %s",
                    Int(i)));
      END;
      assigned := FALSE;
      
      WHILE NOT assigned DO
        FOR w := FIRST(workers^) TO LAST(workers^) DO
          IF workers[w].freeP() THEN
            IF doDebug THEN
              Debug.Out(F("Fsdb.Parse : assigning partial trace file %s to worker %s", Int(i), Int(w)));
            END;
            
            workers[w].task(wdWr[i], fileTab[i], wdPth[i]);
            assigned := TRUE;
            EXIT
          END
        END;

        (* if we get here, all workers were busy on this
           iteration, and we must wait for a signal *)
        
        IF NOT assigned THEN
          LOCK mu DO
            Thread.Wait(mu, d)
          END
        END
      END
    END;

    Debug.Out("Fsdb.Parse: Waiting for workers to finish");
    
    (* wait for workers to be completely done *)
    FOR i := FIRST(workers^) TO LAST(workers^) DO
      (* wait for workers to finish *)
      WHILE NOT workers[i].freeP() DO
        LOCK mu DO
          Thread.Wait(mu, d)
        END
      END;
      (* 6/25/2001
         strange bug with file descriptors getting mangled
         let workers linger, see if that fixes it! *)
      (*
        workers[i].exit();
        EVAL Thread.Join(workers[i].thr)
      *)

    END;

    (* 
       all jobs are assigned AND all workers are done 
       --->
       we are completely done 
    *)

    Debug.Out("Fsdb.Parse: Workers have finished");
  END GenMultiThreaded;


PROCEDURE SetupFileTabs(VAR fileTab : ARRAY OF CardSeq.T;
                        idxMap : CardSeq.T;
                        aNodes : CARDINAL
  ) =
  VAR
    nFiles := NUMBER(fileTab);
  BEGIN
    FOR i := FIRST(fileTab) TO LAST(fileTab) DO
      fileTab[i] := NEW(CardSeq.T).init()
    END;
    FOR i := 0 TO idxMap.size() - 1 DO
      WITH outIdx = idxMap.get(i) DO
        IF i # 0 (* TIME done separately *) AND outIdx # LAST(CARDINAL) THEN
          WITH fileIdx    = NameControl.FileIndex(nFiles,
                                                  aNodes,
                                                  outIdx) DO
            (* note what we're doing here.. we are adding the
               INPUT INDEX of the node to the file list, indexed by the
               hashed OUTPUT INDEX of the node! *)
            IF doDebug THEN
              Debug.Out(F("Adding to fileTab: input index %s to fileTab[%s]",
                          Int(i), Int(fileIdx)))
            END;
            fileTab[fileIdx].addhi(i)
          END
        END
      END
    END;
  END SetupFileTabs;
  
PROCEDURE LoadAllNames(wr            : Wr.T;
                       rd            : Rd.T;
                       loId, hiId    : CARDINAL;
                       names         : TextSeq.T;
                       typeTab       : TextCardSeqTbl.T;
                       dutName       : TEXT;
                       restrictNodes : TextSet.T;
                       restrictRegEx : RegExList.T;
                       VAR idxMap    : CardSeq.T
                       )
  RAISES { TextReader.NoMore }  =
  VAR
        fsdbNames := NEW(TextCardTbl.Default).init();
    duplicates := NEW(TextCardTbl.Default).init();
    line : TEXT;
    BEGIN
      PutCommandG(wr, F("N %s %s", Int(loId), Int(hiId)));
      WITH nameSet = NEW(TextSetDef.T).init() DO
        WHILE GetLineUntilG(rd, "NR", line) DO
          TRY
            WITH reader = NEW(TextReader.T).init(line),
                 idx    = reader.getInt(),
                 nm     = reader.get(),
                 type   = reader.get(),
                 editNm = RenameBack(dutName, EditName(nm)) DO

              StoreByType(typeTab, type, idx);
              (* we also dont do anything with the type right now,
                 we really should check it's something we can work with *)
              
              VAR
                tryNm : TEXT;
                cnt   : CARDINAL := 0;
              BEGIN
                IF duplicates.get(editNm, cnt) THEN
                  (* if we get here, we saw this name before
                     without an extension *)
                  tryNm := editNm & "_" & Int(cnt);
                  INC(cnt)
                ELSE
                  tryNm := editNm
                END;

                WHILE nameSet.insert(tryNm) DO
                  INC(cnt);
                  tryNm := editNm & "_" & Int(cnt);
                END;
                (* here tryNm is unique *)
                
                (* if no duplicates and we get here, cnt = 0 *)
                EVAL duplicates.put(editNm, cnt);

                <*ASSERT tryNm # NIL*>
                Debug.Out(F("fsdbNames.put(%s, %s)", tryNm, Int(idx)));
                
                WITH hadIt = fsdbNames.put(tryNm, idx) DO
                  <*ASSERT NOT hadIt *>
                END

              END
            END
          EXCEPT
            Lex.Error, FloatMode.Trap =>
            Debug.Error(F("Cant parse N response \"%s\"", line))
          END
        END;

        Debug.Out(F("fsdbNames : %s unique %s",
                    Int(fsdbNames.size()),
                    Int(nameSet.size())));

        RemoveZeros(duplicates);

        IF duplicates.size() # 0 THEN
          Debug.Warning(F("%s duplicate names:", Int(duplicates.size())));
          VAR iter := duplicates.iterate();
              nm : TEXT;
              c : CARDINAL;
          BEGIN
            WHILE iter.next(nm, c) DO
              Debug.Warning(F("%s : %s instances", nm, Int(c)))
            END
          END
        END
      END;
      WITH hadIt = fsdbNames.put("TIME", 0)  (* implicit #0 *) DO
        IF hadIt THEN
          Debug.Error("? node 0 not TIME")
        END
      END;

      (* now we have all names loaded up *)

      idxMap := NameControl.MakeIdxMap(fsdbNames,
                                       restrictNodes,
                                       restrictRegEx,
                                       names);

      Debug.Out(F("made idxMap: names.size() %s / active %s",
                  Int(names.size()), Int(NameControl.CountActiveNames(idxMap))));

    END LoadAllNames;

PROCEDURE Parse(wd, ofn       : Pathname.T;
                names         : TextSeq.T;
                maxFiles      : CARDINAL;
                VAR nFiles    : CARDINAL;

                timeScaleFactor,
                timeOffset,
                voltageScaleFactor,
                voltageOffset : LONGREAL;

                dutName       : TEXT;
                 
                fsdbPath      : Pathname.T;
                wait          : BOOLEAN;
                restrictNodes : TextSet.T;
                restrictRegEx : RegExList.T;
                cmdPath       : Pathname.T;
                compressPath  : Pathname.T;
                compressPrec  : LONGREAL;
                threads       : CARDINAL;
                interpolate   : LONGREAL;
                maxTime       : LONGREAL
  )
  RAISES { } = (* lots of errors but they cause program crash, not exception *)

  (* I thought there was a compiler bug here, so I commented out these lines.

     But there does not seem to be a compiler bug, so they should be OK to 
     bring back *)

  (* the idea here is that we use an external program "nanosimrd", which is
     linked with Synopsys's C++ libraries, to read the actual files.

     We communicate with nanosimrd over a pipe.  We send commands and pick up
     responses.  Most of the responses are in ASCII, but for efficiency, some
     are in packed binary format. *)

  PROCEDURE CommandUnload() =
    BEGIN
      PutCommandG(wr, "U");
      EVAL GetResponseG(rd, "UR");
    END CommandUnload;

  PROCEDURE GetTimesteps(fsdbId : CARDINAL; steps : LRSeq.T)
    RAISES { FloatMode.Trap, Lex.Error, TextReader.NoMore } =
    BEGIN
      PutCommandG(wr, F("I %s", Int(fsdbId)));
      WHILE GetLineUntilG(rd, "IR", line) DO
        WITH reader = NEW(TextReader.T).init(line),
             h = reader.getInt(),
             l = reader.getInt(),
             s = FLOAT(h, LONGREAL) * TwoToThe32 + FLOAT(l, LONGREAL),
             t = s * unit DO
          IF FALSE THEN Debug.Out("timestep " & LR(t)) END;
          steps.addhi(t)
        END
      END
    END GetTimesteps;

  PROCEDURE LoadFsdbIds()
    RAISES { FloatMode.Trap, Lex.Error, TextReader.NoMore } =
    BEGIN
      PutCommandG(wr, "S");
      WITH reader    = GetResponseG(rd, "SR") DO
        loId   := reader.getInt();
        hiId   := reader.getInt();
        WITH unitStr = reader.get() DO
          unit   := ParseUnitStr(unitStr);

          IF doDebug THEN
            Debug.Out(F("Got query response lo=%s hi=%s unitStr=\"%s\"",
                        Int(loId), Int(hiId), unitStr))
          END
        END
      END;
    END LoadFsdbIds;

  PROCEDURE LoadNode(fsdbId : CARDINAL) =
    BEGIN
      (* load first node *)
      PutCommandG(wr, F("R %s %s", Int(fsdbId), Int(fsdbId)));
      EVAL GetResponseG(rd, "RR");

      (* load all corresponding signals *)
      PutCommandG(wr, F("L"));
      EVAL GetResponseG(rd, "LR");
    END LoadNode;

  PROCEDURE ChopTimestepsBasedOnMaxTime(max : LONGREAL) =
    BEGIN
      WHILE timesteps.get(timesteps.size() - 1) > max DO
        EVAL timesteps.remhi()
      END
    END ChopTimestepsBasedOnMaxTime;
    
  PROCEDURE ChopTimestepsBasedOnType(type : TEXT)
    RAISES { FloatMode.Trap, Lex.Error, TextReader.NoMore } =

    (* 
       this routine addresses an issue with AUTOSTOP

       if AUTOSTOP is used in xa, the time data is not truncated at the 
       time that AUTOSTOP is triggered, only the simulation data is so 
       truncated.

       Here what we do is track down "some node" that records the 
       stated type type (normally "nanosim_voltage") and use that to 
       truncate the time series.

       It is untested whether this routine crashes if no data is of type
       nanosim_voltage. (But it should not -- it should just not truncate
       the timesteps.)
    *)
    
    BEGIN
      WITH voltSignals = GetIdsByType(typeTab, type) DO
        IF voltSignals.size() # 0 THEN
          WITH lastVolt  = voltSignals.get(voltSignals.size() - 1),
               ts2       = NEW(LRSeq.T).init() DO

            GetTimesteps(lastVolt, ts2);

            IF doDebug THEN
              Debug.Out(F("ChopTimestepsBasedOnType : node fsdbId %s : ts2 %s min %s max %s",
                          Int(lastVolt),
                          Int(ts2.size()),
                          LR(ts2.get(0)),
                          LR(ts2.get(ts2.size()-1))));
            END;

            IF ts2.size() # 0 THEN
              WITH lastVoltTime = ts2.get(ts2.size() - 1) DO
                (* remove timesteps that aren't present 
                   in the voltage waveform *)

                WHILE timesteps.get(timesteps.size() - 1) > lastVoltTime DO
                  EVAL timesteps.remhi()
                END
              END;

              IF doDebug THEN
                Debug.Out(F("post-edit: timesteps %s min %s max %s",
                            Int(timesteps.size()),
                            LR(timesteps.get(0)),
                            LR(timesteps.get(timesteps.size()-1))));
              END;
        
            END
          
          END
        END
      END;
    END ChopTimestepsBasedOnType;
    
  VAR
    stdin      : ProcUtils.Reader;
    stdout     : ProcUtils.Writer;
    completion : ProcUtils.Completion;
    wr         : Wr.T;
    rd         : Rd.T;
    idxMap     : CardSeq.T;

    wdWr       : REF ARRAY OF Wr.T;
    wdPth      : REF ARRAY OF Pathname.T;

    loId, hiId : CARDINAL;
    unit       : LONGREAL;
    line       : TEXT;
    aNodes     : CARDINAL;

    timesteps     := NEW(LRSeq.T).init();
    doInterpolate := interpolate # NoInterpolate;
    typeTab       := NEW(TextCardSeqTbl.Default).init();
    
  BEGIN

    <*FATAL OSError.E*>
    BEGIN
      stdin  := ProcUtils.GimmeWr(wr);
      stdout := ProcUtils.GimmeRd(rd);
    END;

    WITH cmd = cmdPath & " " & fsdbPath DO
      IF doDebug THEN
        Debug.Out(F("Fsdb.Parse running thread with command \"%s\"", cmd))
      END;
      
      completion := ProcUtils.RunText(cmd,
                                      stdin := stdin,
                                      stderr := ProcUtils.Stderr(),
                                      stdout := stdout);
    END;

    TRY
      LoadFsdbIds();

      LoadNode(loId);

      (* get timesteps *)
      GetTimesteps(loId, timesteps);

      CommandUnload();

      LoadAllNames(wr, rd,         (* comms pipes *)
                   loId, hiId,     (* id range *)
                   names,          (* target names data structure *)
                   typeTab,        (* type mapping *)

                   dutName,        (* name of DUT? *)
                   
                   restrictNodes,
                   restrictRegEx,  (* node restrictions *)
                   
                   idxMap);
      
      IF doDebug THEN
        Debug.Out(F("timesteps %s min %s max %s",
                    Int(timesteps.size()),
                    LR(timesteps.get(0)),
                    LR(timesteps.get(timesteps.size()-1))));
      END;

      IF doInterpolate THEN
        InterpolateTimesteps(timesteps, interpolate)
      END;

      ChopTimestepsBasedOnType("nanosim_voltage");
      (* address AUTOSTOP issue (see proc for details) *)

      IF maxTime # LAST(LONGREAL) THEN
        ChopTimestepsBasedOnMaxTime(maxTime)
      END;

      aNodes := NameControl.WriteNames(wd,
                                       ofn,
                                       names,
                                       idxMap,
                                       maxFiles,
                                       nFiles,
                                       wdWr,
                                       wdPth);
      <*ASSERT wdWr # NIL*>

      (* write out timesteps *)
      WriteTimesteps(wdWr^, timesteps, timeScaleFactor, timeOffset, nFiles);
      
      (* let's build the map of what node goes into which file *)
      (* note there are several indices at work here

         we have the index of the node in the fsdb: this is also the
         index of the entry in the idxMap

         we have the index of the node in the output trace: this is also
         the contents of the idxMap

         --

         here what we do is we collate all the indices over the files, and
         then generate one file at a time.

         If we need to and have the CPU power, we can try to parallelize the
         file generation later. 
      *)

      IF doDebug THEN
        Debug.Out(F("Fsdb writing files: nFiles %s aNodes %s",
                    Int(nFiles), Int(aNodes)));
      END;

      WITH fileTab = NEW(REF ARRAY OF CardSeq.T, nFiles) DO

        SetupFileTabs(fileTab^, idxMap, aNodes);
        
        (* start generation threads *)
        VAR
          MultiThreaded := threads # 0;
        BEGIN
          (* now generate the files in turn *)
          IF MultiThreaded THEN
            GenMultiThreaded(threads,
                             idxMap,
                             timesteps,
                             fileTab^,
                             wdWr^,
                             wdPth^,
                             cmdPath, fsdbPath,
                             compressPath,
                             compressPrec,
                             voltageScaleFactor, voltageOffset,
                             interpolate,
                             unit)
            
          ELSE
            GenSingleThreaded(rd, wr,
                             idxMap,
                             timesteps,
                             fileTab^,
                             wdWr^,
                             wdPth^,
                             compressPath,
                             compressPrec,
                             voltageScaleFactor, voltageOffset,
                             interpolate,
                             unit)
          END
        END
      END;

      Debug.Out("Fsdb.Parse sanitizing names.");
      NameControl.SanitizeNames(idxMap, names);

      Debug.Out("Fsdb.Parse closing temp files.");
     
      FOR i := FIRST(wdWr^) TO LAST(wdWr^) DO
        TRY
          Wr.Close(wdWr[i])
        EXCEPT
          Wr.Failure(x) => Debug.Error(F("Trouble closing temp file %s : Wr.Failure : %s", Int(i), AL.Format(x)))
        END
      END;
      Debug.Out("Fsdb.Parse temp files closed.");

    EXCEPT
      FloatMode.Trap, Lex.Error =>
      Debug.Error("Trouble parsing number during Fsdb.Parse conversation")
    |
      TextReader.NoMore =>
      Debug.Error("TextReader.NoMore during Fsdb.Parse conversation")
    END
  END Parse;

PROCEDURE RemoveZeros(tbl : TextCardTbl.T) =
  VAR
    toRemove := NEW(TextSetDef.T).init();
    iter := tbl.iterate();
    t : TEXT;
    c : CARDINAL;
  BEGIN
    WHILE iter.next(t, c) DO
      IF c = 0 THEN
        EVAL toRemove.insert(t)
      END
    END;

    WITH jter = toRemove.iterate() DO
      WHILE jter.next(t) DO
        WITH hadIt = tbl.delete(t, c) DO
          <*ASSERT hadIt*>
        END
      END
    END
  END RemoveZeros;

  (**********************************************************************)

TYPE
  GenClosure = Thread.Closure OBJECT
    mu                : MUTEX;             (* shared between all threads *)
    c, d              : Thread.Condition;  (* shared between all threads *)
    (* c signals new task; d signals new slot *)
    
    tWr               : Wr.T;
    tPath             : Pathname.T;
    nodeIds           : CardSeq.T;
    idxMap            : CardSeq.T;
    nsteps            : CARDINAL;

    cmdPath, fsdbPath : Pathname.T;
    compressPath      : Pathname.T;
    compressPrec      : LONGREAL;
    thr               : Thread.T;
    doExit := FALSE;
    
    voltageScaleFactor,
    voltageOffset,
    interpolate, unit : LONGREAL;
  METHODS
    init(c, d              : Thread.Condition;
         mu                : MUTEX;
         idxMap            : CardSeq.T;
         nsteps            : CARDINAL;
         cmdPath, fsdbPath : Pathname.T;
         compressPath      : Pathname.T;
         compressPrec      : LONGREAL;
         voltageScaleFactor,
         voltageOffset,
         interpolate, unit : LONGREAL;
    ) : GenClosure  := GenInit;
    task(taskWr : Wr.T; taskIds : CardSeq.T; path : Pathname.T)  := GenTask;
    freeP() : BOOLEAN := GenFreeP;
    exit() := GenExit;
  OVERRIDES
    apply := GenApply;
  END;

PROCEDURE GenExit(cl : GenClosure) =
  BEGIN
    LOCK cl.mu DO
      cl.doExit := TRUE;
      Thread.Broadcast(cl.c)
    END
  END GenExit;

PROCEDURE GenInit(cl : GenClosure;
                  c, d : Thread.Condition;
                  mu : MUTEX;
                  idxMap : CardSeq.T;
                  nsteps : CARDINAL;
                  cmdPath, fsdbPath : Pathname.T;
                  compressPath : Pathname.T;
                  compressPrec : LONGREAL;
                  voltageScaleFactor, voltageOffset : LONGREAL;
                  interpolate, unit : LONGREAL
  ) : GenClosure =
  BEGIN
    cl.mu                 := mu;
    cl.c                  := c;
    cl.d                  := d;
    cl.idxMap             := idxMap;
    cl.nsteps             := nsteps;
    cl.cmdPath            := cmdPath;
    cl.fsdbPath           := fsdbPath;
    cl.compressPath       := compressPath;
    cl.compressPrec       := compressPrec;
    cl.thr                := Thread.Fork(cl);
    cl.voltageScaleFactor := voltageScaleFactor;
    cl.voltageOffset      := voltageOffset;
    cl.interpolate        := interpolate;
    cl.unit               := unit;
    RETURN cl
  END GenInit;

  (* 
     the way the task assignment works is that we look for an idle worker
     (one whose tWr is NIL)

     then we set the tWr of that worker to the correct file, with the path
     (for debugging?) and sequence of nodes to be dumped into that file.

  *)
  
PROCEDURE GenTask(cl       : GenClosure;
                  taskWr   : Wr.T;
                  taskIds  : CardSeq.T;
                  taskPath : Pathname.T) =
  BEGIN
    (* assign a task to the given worker / nanosimrd instance *)
    LOCK cl.mu DO
      <*ASSERT cl.tWr = NIL*>
      cl.tWr     := taskWr;
      cl.tPath   := taskPath;
      cl.nodeIds := taskIds
    END;
    Thread.Broadcast(cl.c) (* several workers can be waiting *)
  END GenTask;

PROCEDURE GenFreeP(cl : GenClosure) : BOOLEAN =
  BEGIN
    LOCK cl.mu DO
      RETURN cl.tWr = NIL
    END
  END GenFreeP;

PROCEDURE GenApply(cl : GenClosure) : REFANY =
  (* this apply runs a session with a nanosimrd process *)
  (* we can run multiple in parallel *)

  VAR
    cmdStdin   : ProcUtils.Reader;
    cmdStdout  : ProcUtils.Writer;
    completion : ProcUtils.Completion;
    cmdWr      : Wr.T;
    cmdRd      : Rd.T;
    loId, hiId : CARDINAL;
    unit       : LONGREAL;
  BEGIN
    <*FATAL OSError.E*>
    BEGIN
      cmdStdin  := ProcUtils.GimmeWr(cmdWr);
      cmdStdout := ProcUtils.GimmeRd(cmdRd);
    END;
    
    completion := ProcUtils.RunText(cl.cmdPath & " " & cl.fsdbPath,
                                    stdin  := cmdStdin,
                                    stderr := ProcUtils.Stderr(),
                                    stdout := cmdStdout);
    TRY
      PutCommandG(cmdWr, "S");
      WITH reader    = GetResponseG(cmdRd, "SR") DO
        loId   := reader.getInt();
        hiId   := reader.getInt();
        WITH unitStr = reader.get() DO
          unit   := ParseUnitStr(unitStr);

          IF doDebug THEN
            Debug.Out(F("Got query response lo=%s hi=%s unitStr=\"%s\"",
                        Int(loId), Int(hiId), unitStr))
          END
        END
      END;

      (* memorize times *)
      PutCommandG(cmdWr, F("i %s", Int(loId)));
      EVAL GetResponseG(cmdRd, "iR");      

      (* not yet finished *)
      LOOP
        LOCK cl.mu DO
          WHILE cl.tWr = NIL AND NOT cl.doExit DO
            Thread.Wait(cl.mu, cl.c)
          END;
          IF cl.doExit THEN
            (* make sure worker exits *)
            PutCommandG(cmdWr, "Q");
            EVAL GetResponseG(cmdRd, "QR");
            RETURN NIL
          END;
          <*ASSERT cl.tWr # NIL*>
          (* now we have a request -- execute request *)
        END;

        (* note we cannot hold the lock while we run *)
        GeneratePartialTraceFile(cl.tWr,
                                 cl.nodeIds,
                                 cl.idxMap,
                                 cmdRd,
                                 cmdWr,
                                 cl.nsteps,
                                 cl.compressPath,
                                 cl.compressPrec,
                                 cl.voltageScaleFactor,
                                 cl.voltageOffset,
                                 cl.interpolate,
                                 cl.unit,
                                 cl.tPath
                                 );

        LOCK cl.mu DO
          (* done executing request, mark ourselves as free and signal *)
          cl.tWr     := NIL;
          cl.tPath   := NIL;
          cl.nodeIds := NIL;
        END;
        
        Thread.Signal(cl.d)

      END
      
    EXCEPT
      FloatMode.Trap, Lex.Error =>
      Debug.Error("Trouble parsing number during Fsdb.Parse conversation")
    |
      TextReader.NoMore =>
      Debug.Error("TextReader.NoMore during Fsdb.Parse conversation")
    END; 
    RETURN NIL
  END GenApply;

  (**********************************************************************)

PROCEDURE GeneratePartialTraceFile(wr                   : Wr.T;
                                   fileTab              : CardSeq.T;
                                   (* INPUT indices to process *)
                                   
                                   idxMap               : CardSeq.T;
                                   (* mapping from input to output indices *)

                                   cmdRd                : Rd.T;
                                   cmdWr                : Wr.T;

                                   nSteps               : CARDINAL;
                                   compressPath         : Pathname.T;
                                   (* if NIL, no compression *)
                                   compressPrec         : LONGREAL;
                                   
                                   voltageScaleFactor,
                                   voltageOffset,
                                   interpolate,
                                   unit                 : LONGREAL;
                                   
                                   path                 : Pathname.T;
                                   (* for debugging only? *)
  
  ) =
  BEGIN
    IF doDebug THEN
      Debug.Out(F("GeneratePartialTraceFile : %s indices", Int(fileTab.size())));
    END;

    IF compressPath # NIL THEN
      WITH compressCmdString = F("F %s -dump -filter %s %s %s -prec %s",
                                 compressPath,
                                 Int(nSteps),
                                 LR(interpolate),
                                 LR(unit),
                                 LR(compressPrec)) DO
        IF doDebug THEN
          Debug.Out(F("Setting up waveform compression with string \"%s\"",
                      compressCmdString))
        END;

        PutCommandG(cmdWr, compressCmdString);
        EVAL GetResponseG(cmdRd, "FR");
      END
    END;
    
    (* set up indications of interest *)
    FOR i := 0 TO fileTab.size() - 1 DO
      WITH id = fileTab.get(i) DO
        PutCommandG(cmdWr, F("r %s", Int(id)));
        EVAL GetResponseG(cmdRd, "rR");
      END
    END;

    PutCommandG(cmdWr, "L");
    EVAL GetResponseG(cmdRd, "LR");

    IF interpolate = NoInterpolate THEN
      (* does compression work here? *)
      PutCommandG(cmdWr, "t")
    ELSE
      IF compressPath = NIL THEN
        PutCommandG(cmdWr, "x")
      ELSE
        PutCommandG(cmdWr, "y")
      END
    END;

    FOR i := 0 TO fileTab.size() - 1 DO
      WITH inId  = fileTab.get(i),
           outId = idxMap.get(inId) DO

        IF doDebug THEN
          Debug.Out(F("Expecting node data for inId %s outId %s",
                      Int(inId), Int(outId)));
        END;

        IF compressPath # NIL THEN
          DoCompressedReceive(wr,
                              path,
                              cmdRd,
                              nSteps,
                              voltageScaleFactor,
                              voltageOffset,
                              inId,
                              outId)
        ELSE
          DoUncompressedReceive(wr,
                                path,
                                cmdRd,
                                nSteps,
                                voltageScaleFactor,
                                voltageOffset,
                                interpolate,
                                unit,
                                inId,
                                outId)
        END;
        

      END
    END;

    IF interpolate = NoInterpolate THEN
      EVAL GetResponseG(cmdRd, "tR")
    ELSE
      IF compressPath = NIL THEN

        EVAL GetResponseG(cmdRd, "xR")
      ELSE
        EVAL GetResponseG(cmdRd, "yR")
      END
    END;
    
    PutCommandG(cmdWr, "U");
    EVAL GetResponseG(cmdRd, "UR")
  END GeneratePartialTraceFile;

PROCEDURE DoUncompressedReceive(wr                  : Wr.T;
                                path                : Pathname.T;
                                cmdRd               : Rd.T;
                                nSteps              : CARDINAL;
                                voltageScaleFactor,
                                voltageOffset,
                                interpolate,
                                unit                : LONGREAL;
                                inId, outId         : CARDINAL) =
  VAR
    buff := NEW(REF ARRAY OF LONGREAL, nSteps);
    node : CARDINAL;
  BEGIN
    (* no compression *)
    IF interpolate = NoInterpolate THEN
      ReadBinaryNodeDataG(cmdRd, node, buff^);
    ELSE
      ReadInterpolatedBinaryNodeDataG(cmdRd,
                                      node,
                                      buff^,
                                      interpolate,
                                      unit);
    END;
    
    IF node # inId THEN
      Debug.Error(F("unexpected node %s # inId %s", Int(node), Int(inId)))
    END;
    
    (* write data to temp file in correct format *)
    IF doDebug THEN
      Debug.Out(F("Writing data block outId %s nSteps %s",
                  Int(outId), Int(nSteps)));
    END;
    
    IF voltageScaleFactor # 1.0d0 OR voltageOffset # 0.0d0 THEN
      FOR i := FIRST(buff^) TO LAST(buff^) DO
        buff[i] := voltageScaleFactor * buff[i] + voltageOffset
      END
    END;
    
    TRY
      IF doDebug THEN
        Debug.Out(F("Writing data for outId %s to %s offset %s first %s last %s",
                    Int(outId),
                    path,
                    Int(Wr.Index(wr)),
                    LR(buff[0]),
                    LR(buff[LAST(buff^)])))
      END;
      DataBlock.WriteData(wr, outId, buff^)
    EXCEPT
      Wr.Failure(x) =>
      Debug.Error(F("Wr.Failure writing data for node %s : %s ",
                    Int(outId),
                    AL.Format(x)))
    END
  END DoUncompressedReceive;

PROCEDURE DoCompressedReceive(wr                  : Wr.T;
                              path                : Pathname.T;
                              cmdRd               : Rd.T;
                              nSteps              : CARDINAL;
                              voltageScaleFactor,
                              voltageOffset       : LONGREAL;
                              inId, outId         : CARDINAL) =
  VAR
    node : CARDINAL;
    data : TEXT;
    
    norm : SpiceCompress.Norm;
    (* normalization constants *)
    
  BEGIN
    data := ReadCompressedNodeDataG(cmdRd,
                                    node,
                                    norm);

    
    IF node # inId THEN
      Debug.Error(F("unexpected node %s # inId %s", Int(node), Int(inId)))
    END;
    
    (* write data to temp file in correct format *)
    IF doDebug THEN
      Debug.Out(F("Writing data block outId %s nSteps %s",
                  Int(outId), Int(nSteps)));
    END;
    
    IF voltageScaleFactor # 1.0d0 OR voltageOffset # 0.0d0 THEN
      norm.min := voltageScaleFactor * norm.min + voltageOffset;
      norm.max := voltageScaleFactor * norm.max + voltageOffset;
    END;
    
    TRY
      IF doDebug THEN
        Debug.Out(F("Writing data for outId %s to %s offset %s",
                    Int(outId),
                    path,
                    Int(Wr.Index(wr))))
      END;
      WITH normWr = TextWr.New() DO
        UnsafeWriter.WriteLRA(normWr,
                              ARRAY [0..1] OF LONGREAL { norm.min, norm.max });
        DataBlock.WriteCompressed(wr,
                                  outId,
                                  ARRAY [0..1] OF TEXT { TextWr.ToText(normWr),
                                                         data })
      END
    EXCEPT
      Wr.Failure(x) =>
      Debug.Error(F("Wr.Failure writing data for node %s : %s ",
                    Int(outId),
                    AL.Format(x)))
    END
  END DoCompressedReceive;
  
PROCEDURE TryRemoveSuffix(from        : TEXT;
                          suffix      : TEXT;
                          VAR remains : TEXT) : BOOLEAN =
  BEGIN
    IF TextUtils.HaveSuffix(from, suffix) THEN
      remains := TextUtils.RemoveSuffix(from, suffix);
      RETURN TRUE
    ELSE
      RETURN FALSE
    END
  END TryRemoveSuffix;
  
PROCEDURE ParseUnitStr(unitSpec : TEXT) : LONGREAL =
  VAR numStr : TEXT;
  BEGIN
    FOR i := FIRST(Units) TO LAST(Units) DO
      WITH u = Units[i] DO
        IF TryRemoveSuffix(unitSpec, u.sfx, numStr) OR
           TryRemoveSuffix(unitSpec, u.sfx & "s", numStr) THEN
          TRY
            WITH val = Scan.LongReal(numStr), 
                 res = val * u.val DO
              IF doDebug THEN
                Debug.Out(F("ParseUnitStr \"%s\" : val %s u.val %s res %s",
                            unitSpec,
                            LR(val),
                            LR(u.val),
                            LR(res)));
              END;
              RETURN res
            END
          EXCEPT
            Lex.Error, FloatMode.Trap => (* try again *)
          END
        END
      END
    END;
    Debug.Error("UnitSpec not understood : " & unitSpec);
    <*ASSERT FALSE*>
  END ParseUnitStr;

TYPE
  UnitSuffix = RECORD sfx : TEXT; val : LONGREAL END;

CONST
  Units = ARRAY OF UnitSuffix {
  UnitSuffix { "E" , 1.0d+18 },
  UnitSuffix { "P" , 1.0d+15 },
  UnitSuffix { "T" , 1.0d+12 },
  UnitSuffix { "G" , 1.0d+09 }, 
  UnitSuffix { "M" , 1.0d+06 },
  UnitSuffix { "k" , 1.0d+03 },
  UnitSuffix { "h" , 1.0d+02 },
  UnitSuffix { "da", 1.0d+01 },
  UnitSuffix { ""  , 1.0d-00},
  UnitSuffix { "d" , 1.0d-01 },
  UnitSuffix { "c" , 1.0d-02 },
  UnitSuffix { "m" , 1.0d-03 },
  UnitSuffix { "u" , 1.0d-06 }, (* Greek mu *)
  UnitSuffix { "n" , 1.0d-09 },
  UnitSuffix { "p" , 1.0d-12 },
  UnitSuffix { "f" , 1.0d-15 },
  UnitSuffix { "a" , 1.0d-19 } };
 

BEGIN
  <*ASSERT ParseUnitStr("12")  = 12.0d0*>
  <*ASSERT ParseUnitStr("1da") = 10.0d0*>
  <*ASSERT ParseUnitStr("1M")  = 1.0d6*>
END Fsdb.
