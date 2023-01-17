MODULE Main;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT Debug;
IMPORT Params;
IMPORT AL;
IMPORT OSError;
IMPORT Rd;
IMPORT Pathname;
FROM Fmt IMPORT F, Int, LongReal;
IMPORT Trace;
IMPORT Wr, FileWr;
IMPORT TextWr;
IMPORT Text;
IMPORT UnsafeWriter;
IMPORT FileRd;
IMPORT UnsafeReader;
IMPORT SpiceCompress;
IMPORT Thread;
IMPORT TripleRefTbl;
IMPORT FsdbComms;
IMPORT Matrix;
IMPORT DistZTrace;
IMPORT CardList;

<*FATAL Thread.Alerted*>

CONST
  Usage    = "";
  TE       = Text.Equal;
  LR       = LongReal;

TYPE
  Mode = { ReadBinary, Compress, Filter, ExtractOne };

CONST
  ModeNames = ARRAY Mode OF TEXT { "ReadBinary", "Compress", "Filter", "ExtractOne" };
  
PROCEDURE Lookup(str : TEXT; READONLY a : ARRAY OF TEXT) : CARDINAL =
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      IF TE(str, a[i]) THEN RETURN i END
    END;
    VAR
      str := F("could not find %s among alternatives :");
    BEGIN
      FOR i := FIRST(a) TO LAST(a) DO
        str := str & F( " \"%s\"" , a[i])
      END;
      Debug.Error(str)
    END;
    <*ASSERT FALSE*>
  END Lookup;

TYPE
  FilterData = RECORD
    (* in filtering mode, this is information known by the driver
       program, and that will be passed to us on our command line *)
    npoints     : CARDINAL;
    interpolate : LONGREAL;
    unit        : LONGREAL;
  END;

PROCEDURE OpenTrace() =
  BEGIN
    TRY
      trace := NEW(Trace.T).init(traceRt)
    EXCEPT
      OSError.E(x) =>
      Debug.Error(F("Trouble opening input trace %s : OSError.E : %s",
                    traceRt, AL.Format(x)))
    |
      Rd.Failure(x) =>
      Debug.Error("Trouble opening input trace : Rd.Failure : " & AL.Format(x))
    |
      Rd.EndOfFile =>
      Debug.Error(F("Short read opening input trace"))
    END;
  END OpenTrace;

PROCEDURE GetNode(nodeId : CARDINAL) : REF ARRAY OF LONGREAL =
    VAR
      nSteps := trace.getSteps();
      a      := NEW(REF ARRAY OF LONGREAL, nSteps);
    BEGIN
      TRY
        trace.getNodeData(nodeId, a^)
      EXCEPT
        Rd.Failure(x) =>
        Debug.Error(F("Can't read trace for node id %s : Rd.Failure : %s",
                      Int(nodeId), AL.Format(x)))
      |
        Rd.EndOfFile =>
        Debug.Error(F("Short read reading trace for node id %s",
                      Int(nodeId)))
      END;

      RETURN a
    END GetNode;

  
VAR
  pp                             := NEW(ParseParams.T).init(Stdio.stderr);
  traceRt       : Pathname.T     := "xa";
  doAllDumps    : BOOLEAN;
  relPrec                        := 0.005d0;
  mode          : Mode;
  nodeIds       : CardList.T := NIL;
  trace         : Trace.T;
  outFn, inFn   : Pathname.T     := "-";
  wr : Wr.T;
  doDump        : BOOLEAN;
  fd            : FilterData;
  noArith       : BOOLEAN;
  
BEGIN

  TRY

    noArith := pp.keywordPresent("-noarith");

    doDump := pp.keywordPresent("-dump");
    
    IF pp.keywordPresent("-mode") THEN
      mode := VAL(Lookup(pp.getNext(), ModeNames), Mode)
    END;
    
    doAllDumps := pp.keywordPresent("-dodumpall");
    
    IF pp.keywordPresent("-t") OR pp.keywordPresent("-root") THEN
      traceRt := pp.getNext()
    END;

    IF pp.keywordPresent("-o") THEN
      outFn := pp.getNext()
    END;

    IF pp.keywordPresent("-i") THEN
      inFn := pp.getNext()
    END;

    IF pp.keywordPresent("-prec") THEN
      relPrec := pp.getNextLongReal()
    END;

    WHILE pp.keywordPresent("-n") DO
      nodeIds := CardList.Cons(pp.getNextInt(), nodeIds)
    END;

    IF pp.keywordPresent("-filter") THEN
      mode           := Mode.Filter;
      fd.npoints     := pp.getNextInt();
      fd.interpolate := pp.getNextLongReal();
      fd.unit        := pp.getNextLongReal()
    END;

    pp.finish()
  EXCEPT
    ParseParams.Error =>
    Debug.Error("Can't parse command-line parameters\nUsage: " &
      Params.Get(0) & " " & Usage)
  END;

  (* all modes produce some sort of output! *)

  TRY
    IF TE(outFn, "-") THEN
      wr := Stdio.stdout
    ELSE
      wr := FileWr.Open(outFn)
    END
  EXCEPT
    OSError.E(x) =>
    Debug.Error(F("Can't open output file %s : OSError.E : %s",
                  outFn, AL.Format(x)))
  END;

  (* detailed execution of modes *)
  
  CASE mode OF
    Mode.ReadBinary =>
    OpenTrace();
    VAR
      p := nodeIds;
      t := GetNode(0);
    BEGIN
      WHILE p # NIL DO
        VAR
          a      := GetNode(p.head);
        BEGIN
          TRY
            UnsafeWriter.WriteI  (wr, trace.getSteps());
            UnsafeWriter.WriteLRA(wr, a^);
            
            Wr.Close(wr)
          EXCEPT
            Wr.Failure(x) =>
            Debug.Error(F("Can't write raw trace FOR node id %s : Wr.Failure : %s",
                          Int(p.head), AL.Format(x)))
          END
        END
      END
    END
    
    |

      Mode.ExtractOne =>
      OpenTrace();
      VAR
        p := nodeIds;
        t := GetNode(0);
      BEGIN
        WHILE p # NIL DO
          VAR
            a  := GetNode(p.head);
            wr := FileWr.Open(Int(p.head) & ".dat");
          BEGIN
            FOR i := 0 TO trace.getSteps() - 1 DO
              Wr.PutText(wr, F("%s %s\n", LR(t[i]), LR(a[i])));
            END;
            Wr.Close(wr)
          END;
          p := p.tail
        END
      END
      
      
    |
      
      Mode.Compress =>
    VAR
      rd     : Rd.T;
      nSteps : CARDINAL;
      a      : REF ARRAY OF LONGREAL;
      norm   : SpiceCompress.Norm;
    BEGIN
      TRY
        IF TE(inFn, "-") THEN
          rd := Stdio.stdin
        ELSE
          rd := FileRd.Open(inFn)
        END
      EXCEPT
        OSError.E(x) =>
        Debug.Error(F("Can't open input file %s : OSError.E : %s",
                      outFn, AL.Format(x)))
      END;

      TRY
        nSteps := UnsafeReader.ReadI(rd);
        a      := NEW(REF ARRAY OF LONGREAL, nSteps);
        UnsafeReader.ReadLRA(rd, a^);
      EXCEPT
        Rd.Failure(x) =>
        Debug.Error(F("Trouble reading raw trace from %s : Rd.Failure : %s",
                      inFn, AL.Format(x)))
      |
        Rd.EndOfFile =>
        Debug.Error(F("Short read reading raw trace from %s",
                      inFn))
      END;

      WITH z      = NEW(REF ARRAY OF LONGREAL, nSteps),
           textWr = NEW(TextWr.T).init() DO

        (* first write to mem *)
        TRY
          SpiceCompress.CompressArray("zdebug",
                                      a^,
                                      z^,
                                      relPrec,
                                      doAllDumps,
                                      textWr,
                                      norm,
                                      mem    := NEW(TripleRefTbl.Default).init(),
                                      doDump := doDump)
        EXCEPT
          Matrix.Singular =>
          Debug.Error("Internal error attempting waveform compression : Matrix.Singular")
        END;

        <*FATAL Wr.Failure*>
        BEGIN
          WITH txt = TextWr.ToText(textWr),
               len = Text.Length(txt) DO
            UnsafeWriter.WriteI(wr, len);
            Wr.PutText(wr, txt);
            
            Wr.Close(wr)
          END
        END
      END
    END
  |
    Mode.Filter =>
    (* 
       this is the filter mode for nanosimrd.cpp
       
       any changes here must be synchronized with the appropriate changes
       to nanosimrd.cpp 

       The output format of the compression then needs to be synchronized
       with the trace file format specification, used in tracelib and also
       (eventually) in aplot.
    *)
    VAR
      rd     := Stdio.stdin;
      a      := NEW(REF ARRAY OF LONGREAL, fd.npoints);
      
      nodeid   : CARDINAL;
      finalLen : CARDINAL;
    BEGIN
      FsdbComms.ReadInterpolatedBinaryNodeDataG(rd,
                                                nodeid,
                                                a^,
                                                fd.interpolate,
                                                fd.unit);

      TRY
      DistZTrace.WriteOut(wr,
                          a^,
                          nodeid,
                          doDump,
                          relPrec,
                          doAllDumps,
                          noArith);
      Wr.Close(wr)
      EXCEPT 
          Matrix.Singular =>
          Debug.Error("Internal error attempting waveform compression : Matrix.Singular")
        |
          Wr.Failure(x) =>
            Debug.Error(F("Can't write compressed trace data (%s bytes) : Wr.Failure : %s",
                          Int(finalLen + 1), AL.Format(x)))

      END;
    END
  END
END Main.
