MODULE Trace;

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

<*FATAL Thread.Alerted*>

REVEAL
  T = Public BRANDED Brand OBJECT
    root     : Pathname.T;
    h        : TraceHeader.T;
    tRd      : Rd.T;
    fwdTbl   : TextCardTbl.T;
    revTbl   : CardTextSeqTbl.T;
    timeStep : LONGREAL;
  OVERRIDES
    init        := Init;
    getNodeIdx  := GetNodeIdx;
    getSteps    := GetSteps;
    getTimeStep := GetTimeStep;
    getTimeData := GetTimeData;
    getNodeData := GetNodeData;
    close       := Close;
  END;

PROCEDURE Close(t : T) RAISES { Rd.Failure } =
  BEGIN
    Rd.Close(t.tRd)
  END Close;
  
PROCEDURE Init(t : T; root : Pathname.T) : T
  RAISES { OSError.E, Rd.Failure, Rd.EndOfFile } =
  VAR
    tNam := root & ".trace";
    nNam := root & ".names";
  BEGIN

    t.fwdTbl := NEW(TextCardTbl.Default).init();
    t.revTbl := NEW(CardTextSeqTbl.Default).init();

    EVAL ParseNames(nNam, t.fwdTbl, t.revTbl);
    
    t.tRd := FileRd.Open(tNam);

    t.timeStep := ReadTimeStep(t);

    RETURN t
  END Init;

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

PROCEDURE GetTimeStep(t : T) : LONGREAL =
  BEGIN
    RETURN t.timeStep
  END GetTimeStep;
    
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
