MODULE Trace;

IMPORT TraceRep;

IMPORT Pathname;
IMPORT OSError;
IMPORT TraceHeader;
IMPORT TraceUnsafe;
IMPORT Rd, FileRd;
IMPORT TextCardTbl;
IMPORT CardTextSeqTbl;
IMPORT TextSeq;
IMPORT TextList;
IMPORT TextReader;
IMPORT Debug;
IMPORT Thread;
FROM Fmt IMPORT F, Int; IMPORT Fmt;
IMPORT TextSet, TextSetDef;
IMPORT TraceFile;

<*FATAL Thread.Alerted*>

VAR doDebug := TRUE;

CONST LR = Fmt.LongReal;

REVEAL
  T = TraceRep.Private BRANDED Brand OBJECT
    root     : Pathname.T;
    h        : TraceHeader.T;
    tRd      : Rd.T;
    timeStep : LONGREAL;
  OVERRIDES
    init        := Init;
    getNodeIdx  := GetNodeIdx;
    getSteps    := GetSteps;
    getNodes    := GetNodes;
    getTimeData := GetTimeData;
    getNodeData := GetNodeData;
    getCanonicalName := GetCanonicalName;
    getAliases  := GetAliases;
    allNames    := AllNames;
    close       := Close;
  END;

PROCEDURE Close(t : T) RAISES { Rd.Failure } =
  BEGIN
    Rd.Close(t.tRd)
  END Close;
  
PROCEDURE Init(t : T; root : Pathname.T) : T
  RAISES { OSError.E, Rd.Failure, Rd.EndOfFile } =
  VAR
    tNam : Pathname.T;
    nNam := root & ".names";
    ok : BOOLEAN;
  BEGIN

    t.fwdTbl := NEW(TextCardTbl.Default).init();
    t.revTbl := NEW(CardTextSeqTbl.Default).init();

    EVAL ParseNames(nNam, t.fwdTbl, t.revTbl);

    FOR i := FIRST(TraceFile.Version) TO LAST(TraceFile.Version) DO
      ok := TRUE;
      tNam := root & "." & TraceFile.VersionSuffixes[i];
      TRY
        t.tRd := FileRd.Open(tNam);
      EXCEPT
        OSError.E => ok := FALSE
      END;

      IF ok THEN
        ok :=  AttemptParse[i](t)
      END;
      IF ok THEN
        EXIT
      ELSE
        (* no good -- clean up *)
        Rd.Close(t.tRd);
        t.tRd := NIL
      END
    END;

    IF ok THEN
      RETURN t
    ELSE
      Debug.Error("Trace.Init unable to parse for trace root " & root);
      <*ASSERT FALSE*>
    END
  END Init;

  (**********************************************************************)

TYPE Parser = PROCEDURE(t : T) : BOOLEAN RAISES { Rd.Failure, Rd.EndOfFile };

CONST  AttemptParse = ARRAY TraceFile.Version OF Parser {
  AttemptParseUnreordered,
  AttemptParseReordered,
  AttemptParseCompressedV1 };

PROCEDURE AttemptParseUnreordered(<*UNUSED*>t : T) : BOOLEAN =
  BEGIN
    RETURN FALSE
  END AttemptParseUnreordered;

PROCEDURE AttemptParseReordered(t : T) : BOOLEAN
  RAISES { Rd.Failure, Rd.EndOfFile } =
  BEGIN

    WITH hh = TraceFile.ReadHeader(t.tRd) DO
      IF hh.version # TraceFile.Version.Reordered THEN
        RETURN FALSE
      END
    END;

    Rd.Seek(t.tRd, 0);
    
    t.h := TraceUnsafe.GetHeader(t.tRd, t.revTbl.size());

    (* need to check format *)
    
    t.timeStep := ReadTimeStep(t);

    IF doDebug THEN
      Debug.Out(F("TraceHeader start %s, end %s, steps %s, nNodes %s",
                  Int(t.h.start), Int(t.h.end), Int(t.h.steps), Int(t.h.nNodes)));
      Debug.Out(F("timestep %s", LR(t.timeStep)))
    END;
    RETURN TRUE
  END AttemptParseReordered;

PROCEDURE AttemptParseCompressedV1(t : T) : BOOLEAN =
  BEGIN
    WITH hh = TraceFile.ReadHeader(t.tRd) DO
      IF hh.version # TraceFile.Version.CompressedV1 THEN
        RETURN FALSE
      END
    END;
  END AttemptParseCompressedV1;

  (**********************************************************************)
  
PROCEDURE GetNodes(t : T) : CARDINAL =
  BEGIN
    RETURN t.revTbl.size()
  END GetNodes;

PROCEDURE GetCanonicalName(t : T; idx : CARDINAL) : TEXT =
  VAR
    seq : TextSeq.T;
  BEGIN
    WITH hadIt = t.revTbl.get(idx, seq) DO
      <*ASSERT hadIt*>
      
      RETURN seq.get(0)
    END
  END GetCanonicalName;

PROCEDURE GetAliases(t : T; idx : CARDINAL) : TextSet.T =
  VAR
    res := NEW(TextSetDef.T).init();
  BEGIN
    GetAliasesInt(t, idx, res);
    RETURN res
  END GetAliases;
  
PROCEDURE GetAliasesInt(t : T; idx : CARDINAL; res : TextSet.T) =
  VAR
    seq : TextSeq.T;
  BEGIN
    WITH hadIt = t.revTbl.get(idx, seq) DO
      <*ASSERT hadIt*>
      
      FOR i := 0 TO seq.size() - 1 DO
        EVAL res.insert(seq.get(i))
      END
    END
  END GetAliasesInt;

PROCEDURE AllNames(t : T) : TextSet.T =
  VAR
    res := NEW(TextSetDef.T).init();
  BEGIN
    FOR i := 0 TO t.revTbl.size() - 1 DO
      GetAliasesInt(t, i, res)
    END;
    RETURN res
  END AllNames;
 
PROCEDURE ParseNames(nNam   : Pathname.T;
                     fwdTbl : TextCardTbl.T;
                     revTbl : CardTextSeqTbl.T) : CARDINAL
  RAISES { OSError.E, Rd.Failure } =
  VAR
    rd := FileRd.Open(nNam);
    p : TextList.T;
    id := 0;
  BEGIN
    TRY
      LOOP
        WITH line   = Rd.GetLine(rd),
             nr     = NEW(TextReader.T).init(line),
             names  = nr.shatter("=", ""),
             seq    = NEW(TextSeq.T).init() DO

          p := names;
          WHILE p # NIL DO
            seq.addhi(p.head);
            WITH hadIt = fwdTbl.put(p.head, id) DO
              IF hadIt THEN
                Debug.Warning(nNam & " : duplicate mapping for node \"" &
                  p.head & "\"")
              END
            END;
            p := p.tail
          END;
          EVAL revTbl.put(id, seq);
          INC(id)
        END
      END
    EXCEPT
      Rd.EndOfFile =>
    END;
    Rd.Close(rd);
    IF doDebug THEN
      Debug.Out(F("Trace.ParseNames : %s nodes, %s names",
                  Int(revTbl.size()), Int(fwdTbl.size())))
    END;
    RETURN id (* number of nodes *)
  END ParseNames;
  
PROCEDURE GetNodeIdx(t : T; node : TEXT; VAR idx : CARDINAL) : BOOLEAN =
  BEGIN
    RETURN t.fwdTbl.get(node, idx)
  END GetNodeIdx;
  
PROCEDURE GetSteps(t : T) : CARDINAL =
  BEGIN
    RETURN t.h.steps
  END GetSteps;

PROCEDURE ReadTimeStep(t : T) : LONGREAL
  RAISES { Rd.Failure, Rd.EndOfFile } =
  VAR
    a : ARRAY [0..1] OF LONGREAL;
  BEGIN
    GetTimeData(t, a);
    RETURN a[1] - a[0]
  END ReadTimeStep;

PROCEDURE GetTimeData(t : T; VAR timea : ARRAY OF LONGREAL)
  RAISES { Rd.Failure, Rd.EndOfFile } =
  BEGIN
    GetNodeData(t, 0, timea)
  END GetTimeData;

PROCEDURE GetNodeData(t : T; idx : CARDINAL; VAR arr : ARRAY OF LONGREAL)
  RAISES { Rd.Failure, Rd.EndOfFile } =
  BEGIN
    TraceUnsafe.GetDataArray(t.tRd, t.h, idx, arr)
  END GetNodeData;

BEGIN END Trace.
