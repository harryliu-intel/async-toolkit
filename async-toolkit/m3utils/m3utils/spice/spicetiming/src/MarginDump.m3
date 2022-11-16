MODULE MarginDump;
IMPORT MarginMeasurementSeq;
IMPORT Debug;
FROM Fmt IMPORT F, Int, LongReal;
IMPORT TextSetDef;
IMPORT CheckDirSetDef;
IMPORT CheckDir;
IMPORT MarginMeasurementArraySort;
IMPORT MarginMeasurement;
IMPORT Text;
IMPORT FileWr, Wr;
IMPORT MarginScenario;
IMPORT Thread;

<*FATAL Thread.Alerted*>

CONST LR = LongReal;
      TE = Text.Equal;
      
PROCEDURE Do(seq : MarginMeasurementSeq.T; Nworst : CARDINAL) : MarginMeasurementSeq.T =
  VAR
    allTags := NEW(TextSetDef.T).init();
    allDatDirs := NEW(CheckDirSetDef.T).init();
    allClkDirs := CheckDir.T {};
    arr := NEW(REF ARRAY OF MarginMeasurement.T, seq.size());

    res := NEW(MarginMeasurementSeq.T).init();
  BEGIN
    Debug.Out(F("MarginDump.Do: %s measurements", Int(seq.size())));

    (* record all *)

    FOR i := 0 TO seq.size() - 1 DO
      WITH sc = seq.get(i).scenario DO
        EVAL allTags.insert(sc.tag);
        EVAL allDatDirs.insert(sc.datDir);
        allClkDirs := allClkDirs + CheckDir.T { sc.clkDir };
      END
    END;
    
    (* let's start by splitting by tags *)

    VAR
      iter := allTags.iterate();
      tag : TEXT;
    BEGIN
      WHILE iter.next(tag) DO
        WITH n = FilterByTag(seq, tag, arr^),
             sub = SUBARRAY(arr^, 0, n) DO
          MarginMeasurementArraySort.Sort(sub);
          DumpOne(tag, sub);
          FOR i := 0 TO MIN(Nworst, NUMBER(sub)) - 1 DO
            res.addhi(sub[i])
          END
        END
      END
    END;

    RETURN res
  END Do;

PROCEDURE FilterByTag(seq     : MarginMeasurementSeq.T;
                      tag     : TEXT;
                      VAR arr : ARRAY OF MarginMeasurement.T) : CARDINAL =
  VAR
    j := 0;
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      WITH m = seq.get(i) DO
        IF TE(m.scenario.tag, tag) THEN
          arr[j] := m;
          INC(j)
        END
      END
    END;
    RETURN j
  END FilterByTag;

PROCEDURE DumpOne(nm : TEXT; READONLY a : ARRAY OF MarginMeasurement.T) =
  VAR
    fn := nm & ".dat";
    wr := FileWr.Open(fn);
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      WITH m = a[i] DO
        Wr.PutText(wr,
                   F("%s, %s, %s\n",
                     LR(m.margin),
                     MarginScenario.Format(m.scenario),
                     LR(m.at)))
      END
    END;
    Wr.Close(wr)
  END DumpOne;
  
BEGIN END MarginDump.
