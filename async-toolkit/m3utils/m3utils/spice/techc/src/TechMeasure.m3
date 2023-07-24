MODULE TechMeasure;
IMPORT Pathname;
IMPORT Trace;
IMPORT Debug;
FROM Fmt IMPORT F, LongReal, Int, FN;
IMPORT OSError;
IMPORT Rd;
IMPORT AL;
IMPORT Wr;
IMPORT FileWr;
IMPORT LongRealSeq;
IMPORT Thread;
IMPORT TechConfig;
IMPORT TransitionFinder;

FROM TechConfig IMPORT TranNames, ModeNames, SimuNames, CornNames,
                       GateNames, TechNames, Gate;

<*FATAL Thread.Alerted*>

CONST LR = LongReal;

PROCEDURE DoMeasure(READONLY c : TechConfig.T;
                    traceRoot, outName, workDir : Pathname.T;
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
                LR(timeData[1] - timeData[0])));
    
    CONST
      StartTime      = 12.0d-9;
      EarlyStartTime =  1.0d-9;
      StartTran = 1;
    VAR
      xIdx := GetIdx(trace, "x[0]");
      iIdx := GetIdx(trace, "vissx");
      yIdx := GetIdx(trace, "vissy");
      cycle, meancurrent, leakcurrent, latency : LONGREAL;
    BEGIN
      TRY
        trace.getNodeData(xIdx, nodeData^);
      EXCEPT
        Rd.Failure, Rd.EndOfFile => Debug.Error("Trouble reading node data")
      END;
      
      cycle := CycleTime(timeData^, nodeData^,
                         c.volt / 2.0d0, StartTime, StartTran, StartTran + 1);

      latency := HighTime(timeData^, nodeData^,
                          c.volt / 2.0d0, c.volt / 10.0d0,
                          EarlyStartTime, 0);
      
      Debug.Out("Measured cycle   time " & LR(cycle));
      Debug.Out("Measured latency time " & LR(latency));

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
          timeResult : LONGREAL;
        BEGIN
          CASE c.gate OF
            Gate.Xor_Z1_0p0sigma,  
            Gate.Xor_Z1_5p3sigma,
            Gate.Xor_Z2_0p0sigma,  
            Gate.Xor_Z2_5p3sigma,

            Gate.Aoi_Z1_0p0sigma,  
            Gate.Aoi_Z1_5p3sigma,
            Gate.Aoi_Z2_0p0sigma,  
            Gate.Aoi_Z2_5p3sigma
            =>
            timeResult := latency
          ELSE
            timeResult := cycle
          END;
            
          Wr.PutText(wr,
                     FN("%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n",
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
                           LR(timeResult),
                           LR(meancurrent),
                           LR(leakcurrent),
                           workDir,
                           c.hspiceModelName
                           }));
          Wr.Close(wr);
          RETURN timeResult < 1.0d10  (* measure time is less than 
                                         10^10 seconds,
                                         i.e., it exists *)
        END;

        
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

PROCEDURE HighTime(READONLY timea, nodea : ARRAY OF LONGREAL;
                   cross                 : LONGREAL;
                   hyst                  : LONGREAL;
                   startTime             : LONGREAL;
                   startTran             : CARDINAL) : LONGREAL =
  VAR
    tseq := TransitionFinder.Find(timea, nodea, cross, hyst);
    idx := 0;
    res := LAST(LONGREAL);
  BEGIN
    FOR i := 0 TO tseq.size() - 1 DO
      WITH tran = tseq.get(i) DO
        IF tran.at >= startTime THEN
          IF tran.dir = 1 THEN
            IF idx # startTran THEN
              INC(idx)
            ELSE
              (* this is the transition we care about *)
              IF i + 1 < tseq.size() THEN
                res := tseq.get(i + 1).at - tran.at
              END;
              EXIT
            END
          END
        END
      END
    END;
    RETURN res
  END HighTime;

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

BEGIN END TechMeasure.
