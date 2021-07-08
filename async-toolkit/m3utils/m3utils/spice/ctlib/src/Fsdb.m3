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
IMPORT TextUtils, Lex, FloatMode;
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
IMPORT TextSetDef;
IMPORT DataBlock;
IMPORT TextCardTbl;

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
      Debug.Out(F("Fsdb.Parse.PutCommand \"%s\"", cmd));
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
          Debug.Out(F("Fsdb.Parse.GetResponse \"%s\"", line));
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

PROCEDURE ReadBinaryNodeDataG(rd : Rd.T;
                              VAR nodeid : CARDINAL;
                              VAR buff : ARRAY OF LONGREAL) =
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
                threads       : CARDINAL
  )
  RAISES { } = (* lots of errors but they cause program crash, not exception *)
  

(*    
  PROCEDURE PutCommand(cmd : TEXT) =
    BEGIN PutCommandG(wr, cmd) END PutCommand;

  PROCEDURE GetResponse(matchKw : TEXT) : TextReader.T =
    BEGIN RETURN GetResponseG(rd, matchKw) END GetResponse;

  PROCEDURE ReadBinaryNodeData(VAR nodeid : CARDINAL;
                               VAR buff : ARRAY OF LONGREAL) =
    BEGIN ReadBinaryNodeDataG(rd, nodeid, buff) END ReadBinaryNodeData;

  PROCEDURE GetLineUntil(term : TEXT; VAR line : TEXT) : BOOLEAN =
    BEGIN RETURN GetLineUntilG(rd, term, line) END GetLineUntil;
*)

  VAR
    stdin      : ProcUtils.Reader;
    stdout     : ProcUtils.Writer;
    completion : ProcUtils.Completion;
    wr : Wr.T;
    rd : Rd.T;
    idxMap : CardSeq.T;

    wdWr : REF ARRAY OF Wr.T;

    loId, hiId : CARDINAL;
    unit : LONGREAL;
    line : TEXT;
    timesteps := NEW(LRSeq.T).init();
    aNodes : CARDINAL;

    duplicates := NEW(TextCardTbl.Default).init();
    
  CONST
    TwoToThe32 = FLOAT(Word.Shift(1, 32), LONGREAL);
  BEGIN

    <*FATAL OSError.E*>
    BEGIN
      stdin  := ProcUtils.GimmeWr(wr);
      stdout := ProcUtils.GimmeRd(rd);
    END;
    
    completion := ProcUtils.RunText(cmdPath & " " & fsdbPath,
                                    stdin := stdin,
                                    stderr := ProcUtils.Stderr(),
                                    stdout := stdout);


    TRY
      PutCommandG(wr, "S");
      WITH reader    = GetResponseG(rd, "SR") DO
        loId   := reader.getInt();
        hiId   := reader.getInt();
        WITH unitStr = reader.get() DO
          unit   := ParseUnitStr(unitStr);
          
          Debug.Out(F("Got query response lo=%s hi=%s unitStr=\"%s\"",
                      Int(loId), Int(hiId), unitStr))
        END
      END;

      (* load first node *)
      PutCommandG(wr, F("R %s %s", Int(loId), Int(loId)));
      EVAL GetResponseG(rd, "RR");

      PutCommandG(wr, F("L"));
      EVAL GetResponseG(rd, "LR");

      (* get timesteps *)
      PutCommandG(wr, F("I %s", Int(loId)));
      WHILE GetLineUntilG(rd, "IR", line) DO
        WITH reader = NEW(TextReader.T).init(line),
             h = reader.getInt(),
             l = reader.getInt(),
             s = FLOAT(h, LONGREAL) * TwoToThe32 + FLOAT(l, LONGREAL),
             t = s * unit DO
          IF FALSE THEN Debug.Out("timestep " & LR(t)) END;
          timesteps.addhi(t)
        END
      END;

      Debug.Out(F("timesteps %s min %s max %s",
                  Int(timesteps.size()),
                  LR(timesteps.get(0)),
                  LR(timesteps.get(timesteps.size()-1))));

      PutCommandG(wr, "U");
      EVAL GetResponseG(rd, "UR");

      PutCommandG(wr, F("N %s %s", Int(loId), Int(hiId)));
      WITH nameSet = NEW(TextSetDef.T).init() DO
        WHILE GetLineUntilG(rd, "NR", line) DO
          TRY
            WITH reader = NEW(TextReader.T).init(line),
                 idx    = reader.getInt(),
                 nm     = reader.get(),
                 type   = reader.get(),
                 editNm = RenameBack(dutName, EditName(nm)) DO

              (* we don't use idx because we assume nodes are coming out
                 in exactly reverse order and a complete list of them

                 probably not quite right to do it this way if the fsdb
                 reader ever starts producing a starting node # 1 *)

              (* we also dont do anything with the type right now,
                 we really should check it's something we can work with *)
              
              VAR
                tryNm := editNm;
                cnt : CARDINAL := 0;
              BEGIN
                IF duplicates.get(editNm, cnt) THEN
                  tryNm := editNm & "_" & Int(cnt);
                  WHILE nameSet.insert(tryNm) DO
                    INC(cnt);
                    tryNm := editNm & "_" & Int(cnt);
                  END
                END;

                EVAL duplicates.put(editNm, cnt);

                names.addlo(tryNm)
              END
            END
          EXCEPT
            Lex.Error, FloatMode.Trap =>
            Debug.Error(F("Cant parse N response \"%s\"", line))
          END
        END;

        Debug.Out(F("names : %s unique %s first %s last %s ",
                    Int(names.size()),
                    Int(nameSet.size()),
                    names.get(0),
                    names.get(names.size()-1)));

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
      names.addlo("TIME"); (* implicit #0 *)


      (* now we have all names loaded up *)

      idxMap := NameControl.MakeIdxMap(names, restrictNodes, restrictRegEx);

      Debug.Out(F("made idxMap: names.size() %s / active %s",
                  Int(names.size()), Int(NameControl.CountActiveNames(idxMap))));

      aNodes := NameControl.WriteNames(wd,
                                       ofn,
                                       names,
                                       idxMap,
                                       maxFiles,
                                       nFiles,
                                       wdWr);
      <*ASSERT wdWr # NIL*>

      (* write out timesteps *)
      VAR
        arr := NEW(REF ARRAY OF LONGREAL, timesteps.size());
      BEGIN
        Debug.Out(F("Writing timesteps, steps %s", Int(timesteps.size())));
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
      END;


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

      Debug.Out(F("Fsdb writing files: nFiles %s aNodes %s",
                  Int(nFiles), Int(aNodes)));

      WITH fileTab = NEW(REF ARRAY OF CardSeq.T, nFiles) DO
        FOR i := FIRST(fileTab^) TO LAST(fileTab^) DO
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

        (* start generation threads *)
        VAR
          MultiThreaded := threads # 0;
        BEGIN
          (* now generate the files in turn *)
          IF MultiThreaded THEN
            
            VAR
              workers := NEW(REF ARRAY OF GenClosure, threads);
              c, d := NEW(Thread.Condition);
              mu := NEW(MUTEX);
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
                                                   voltageScaleFactor,
                                                   voltageOffset)
              END;
              
              FOR i := FIRST(fileTab^) TO LAST(fileTab^) DO
                Debug.Out(F("Fsdb.Parse : Generating partial trace file %s",
                            Int(i)));
                assigned := FALSE;
                
                WHILE NOT assigned DO
                  FOR w := FIRST(workers^) TO LAST(workers^) DO
                    IF workers[w].freeP() THEN
                      Debug.Out(F("Fsdb.Parse : assigning partial trace file %s to worker %s", Int(i), Int(w)));
                      
                      workers[w].task(wdWr[i], fileTab[i]);
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

              Debug.Out("Waiting for workers to finish");
              
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
              END

              (* 
                 all jobs are assigned AND all workers are done 
                 --->
                 we are completely done 
              *)
              
            END(*VAR*)
            
          ELSE
            FOR i := FIRST(fileTab^) TO LAST(fileTab^) DO
              Debug.Out(F("Fsdb.Parse : Generating partial trace file %s",
                          Int(i)));
              
              GeneratePartialTraceFile(wdWr[i],
                                       fileTab[i],
                                       idxMap,
                                       rd,
                                       wr, 
                                       timesteps.size(),
                                       voltageScaleFactor,
                                       voltageOffset)
            END
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
    mu      : MUTEX;             (* one per thread *)
    c, d    : Thread.Condition;  (* shared between all threads *)
    (* c signals new task; d signals new slot *)
    
    tWr     : Wr.T;             
    nodeIds : CardSeq.T;
    idxMap  : CardSeq.T;
    nsteps  : CARDINAL;

    cmdPath, fsdbPath : Pathname.T;
    thr     : Thread.T;
    doExit := FALSE;
    
    voltageScaleFactor, voltageOffset : LONGREAL                                   ;
  METHODS
    init(c, d : Thread.Condition;
         mu : MUTEX;
         idxMap : CardSeq.T;
         nsteps : CARDINAL;
         cmdPath, fsdbPath : Pathname.T;
         voltageScaleFactor, voltageOffset : LONGREAL
    ) : GenClosure  := GenInit;
    task(taskWr : Wr.T; taskIds : CardSeq.T)  := GenTask;
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
                  voltageScaleFactor, voltageOffset : LONGREAL
  ) : GenClosure =
  BEGIN
    cl.mu := mu;
    cl.c := c;
    cl.d := d;
    cl.idxMap := idxMap;
    cl.nsteps := nsteps;
    cl.cmdPath := cmdPath;
    cl.fsdbPath := fsdbPath;
    cl.thr := Thread.Fork(cl);
    cl.voltageScaleFactor := voltageScaleFactor;
    cl.voltageOffset := voltageOffset;
    RETURN cl
  END GenInit;

PROCEDURE GenTask(cl : GenClosure; taskWr : Wr.T; taskIds : CardSeq.T) =
  BEGIN
    LOCK cl.mu DO
      <*ASSERT cl.tWr = NIL*>
      cl.tWr := taskWr;
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

  (*
  PROCEDURE PutCommand(cmd : TEXT) =
    BEGIN PutCommandG(cmdWr, cmd) END PutCommand;

  PROCEDURE GetResponse(matchKw : TEXT) : TextReader.T =
    BEGIN RETURN GetResponseG(cmdRd, matchKw) END GetResponse;

  PROCEDURE ReadBinaryNodeData(VAR nodeid : CARDINAL;
                               VAR buff : ARRAY OF LONGREAL) =
    BEGIN ReadBinaryNodeDataG(cmdRd, nodeid, buff) END ReadBinaryNodeData;

  PROCEDURE GetLineUntil(term : TEXT; VAR line : TEXT) : BOOLEAN =
    BEGIN RETURN GetLineUntilG(cmdRd, term, line) END GetLineUntil;
    
  *)
  
  VAR
    cmdStdin : ProcUtils.Reader;
    cmdStdout : ProcUtils.Writer;
    completion : ProcUtils.Completion;
    cmdWr : Wr.T;
    cmdRd : Rd.T;
    loId, hiId : CARDINAL;
    unit : LONGREAL;
  BEGIN
    <*FATAL OSError.E*>
    BEGIN
      cmdStdin  := ProcUtils.GimmeWr(cmdWr);
      cmdStdout := ProcUtils.GimmeRd(cmdRd);
    END;
    
    completion := ProcUtils.RunText(cl.cmdPath & " " & cl.fsdbPath,
                                    stdin := cmdStdin,
                                    stderr := ProcUtils.Stderr(),
                                    stdout := cmdStdout);
    TRY
      PutCommandG(cmdWr, "S");
      WITH reader    = GetResponseG(cmdRd, "SR") DO
        loId   := reader.getInt();
        hiId   := reader.getInt();
        WITH unitStr = reader.get() DO
          unit   := ParseUnitStr(unitStr);
          
          Debug.Out(F("Got query response lo=%s hi=%s unitStr=\"%s\"",
                      Int(loId), Int(hiId), unitStr))
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
                                 cl.voltageScaleFactor,
                                 cl.voltageOffset
                                 );

        LOCK cl.mu DO
          (* done executing request, mark ourselves as free and signal *)
          cl.tWr     := NIL;
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

PROCEDURE GeneratePartialTraceFile(wr      : Wr.T;
                                   fileTab : CardSeq.T;
                                   (* INPUT indices to process *)
                                   
                                   idxMap  : CardSeq.T;
                                   (* mapping from input to output indices *)

                                   cmdRd : Rd.T;
                                   cmdWr : Wr.T;

                                   nSteps : CARDINAL;
                                   voltageScaleFactor, voltageOffset : LONGREAL
                                   ) =
  VAR
    buff := NEW(REF ARRAY OF LONGREAL, nSteps);
    node : CARDINAL;
    
  BEGIN
    Debug.Out(F("GeneratePartialTraceFile : %s indices", Int(fileTab.size())));
    
    (* set up indications of interest *)
    FOR i := 0 TO fileTab.size() - 1 DO
      WITH id = fileTab.get(i) DO
        PutCommandG(cmdWr, F("r %s", Int(id)));
        EVAL GetResponseG(cmdRd, "rR");
      END
    END;

    PutCommandG(cmdWr, "L");
    EVAL GetResponseG(cmdRd, "LR");
    
    PutCommandG(cmdWr, "t");

    FOR i := 0 TO fileTab.size() - 1 DO
      WITH inId  = fileTab.get(i),
           outId = idxMap.get(inId) DO

        Debug.Out(F("Expecting node data for inId %s outId %s",
                    Int(inId), Int(outId)));
        
        ReadBinaryNodeDataG(cmdRd, node, buff^);
        IF node # inId THEN
          Debug.Error(F("unexpected node %s # inId %s", Int(node), Int(inId)))
        END;

        (* write data to temp file in correct format *)
        Debug.Out(F("Writing data block outId %s nSteps %s",
                    Int(outId), Int(nSteps)));

        IF voltageScaleFactor # 1.0d0 OR voltageOffset # 0.0d0 THEN
          FOR i := FIRST(buff^) TO LAST(buff^) DO
            buff[i] := voltageScaleFactor * buff[i] + voltageOffset
          END
        END;

        TRY
          DataBlock.WriteData(wr, outId, buff^)
        EXCEPT
          Wr.Failure(x) =>
          Debug.Error(F("Wr.Failure writing data for node %s : %s ",
                        Int(outId),
                        AL.Format(x)))
        END

      END
    END;

    EVAL GetResponseG(cmdRd, "tR");
    
    PutCommandG(cmdWr, "U");
    EVAL GetResponseG(cmdRd, "UR")
  END GeneratePartialTraceFile;
  
PROCEDURE ParseUnitStr(unitSpec : TEXT) : LONGREAL =
  BEGIN
    FOR i := FIRST(Units) TO LAST(Units) DO
      WITH u = Units[i] DO
        IF TextUtils.HaveSuffix(unitSpec, u.sfx) THEN
          TRY
            WITH val = Scan.LongReal(TextUtils.RemoveSuffix(unitSpec, u.sfx)),
                 res = val * u.val DO
              Debug.Out(F("ParseUnitStr \"%s\" : val %s u.val %s res %s",
                          unitSpec,
                          LR(val),
                          LR(u.val),
                          LR(res)));
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
  <*ASSERT ParseUnitStr("12") = 12.0d0*>
  <*ASSERT ParseUnitStr("1da") = 10.0d0*>
  <*ASSERT ParseUnitStr("1M") = 1.0d6*>
END Fsdb.
