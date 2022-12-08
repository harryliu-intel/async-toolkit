MODULE Main;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT Debug;
IMPORT Params;
IMPORT AL;
IMPORT OSError;
IMPORT Rd;
IMPORT Pathname;
FROM Fmt IMPORT F;
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

<*FATAL Thread.Alerted*>

CONST
  Usage    = "";
  TE       = Text.Equal;

TYPE
  Mode = { ReadBinary, Compress };

CONST
  ModeNames = ARRAY Mode OF TEXT { "ReadBinary", "Compress" };
  
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

VAR
  pp                             := NEW(ParseParams.T).init(Stdio.stderr);
  traceRt       : Pathname.T     := "xa";
  doAllDumps    : BOOLEAN;
  relPrec                        := 0.005d0;
  mode          : Mode;
  nodeId        : CARDINAL;
  trace         : Trace.T;
  outFn, inFn   : Pathname.T     := "-";
  wr : Wr.T;
  doDump        : BOOLEAN;
  
BEGIN

  TRY

    doDump := pp.keywordPresent("-dump");
    
    IF pp.keywordPresent("-mode") THEN
      mode := VAL(Lookup(pp.getNext(), ModeNames), Mode)
    END;
    
    doAllDumps := pp.keywordPresent("-dodumpall");
    
    IF pp.keywordPresent("-t") THEN
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

    IF pp.keywordPresent("-n") THEN
      nodeId := pp.getNextInt()
    END;
    
  EXCEPT
    ParseParams.Error =>
    Debug.Error("Can't parse command-line parameters\nUsage: " &
      Params.Get(0) & " " & Usage)
  END;

  (* all modes produce some sort of output! *)
  
  IF TE(outFn, "-") THEN
    wr := Stdio.stdout
  ELSE
    wr := FileWr.Open(outFn)
  END;

  (* detailed execution of modes *)
  
  CASE mode OF
    Mode.ReadBinary =>
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

    VAR
      nSteps := trace.getSteps();
      a      := NEW(REF ARRAY OF LONGREAL, nSteps);
    BEGIN
      trace.getNodeData(nodeId, a^);
      
      UnsafeWriter.WriteI  (wr, nSteps);
      UnsafeWriter.WriteLRA(wr, a^);

      Wr.Close(wr)
    END
  |
    Mode.Compress =>
    VAR
      rd : Rd.T;
      nSteps : CARDINAL;
      a : REF ARRAY OF LONGREAL;
    BEGIN
      IF TE(inFn, "-") THEN
        rd := Stdio.stdin
      ELSE
        rd := FileRd.Open(inFn)
      END;
      nSteps := UnsafeReader.ReadI(rd);
      a := NEW(REF ARRAY OF LONGREAL, nSteps);
      UnsafeReader.ReadLRA(rd, a^);

      WITH z      = NEW(REF ARRAY OF LONGREAL, nSteps),
           textWr = NEW(TextWr.T).init() DO

        (* first write to mem *)
        
        SpiceCompress.CompressArray("zdebug",
                                    a^,
                                    z^,
                                    relPrec,
                                    doAllDumps,
                                    textWr,
                                    mem := NEW(TripleRefTbl.Default).init(),
                                    doDump := doDump);

        WITH txt = TextWr.ToText(textWr),
             len = Text.Length(txt) DO
          UnsafeWriter.WriteI(wr, len);
          Wr.PutText(wr, txt);

          Wr.Close(wr)
        END
      END
    END
  END;
  

END Main.
