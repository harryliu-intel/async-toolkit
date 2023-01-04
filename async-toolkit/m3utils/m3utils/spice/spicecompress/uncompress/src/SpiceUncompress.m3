MODULE SpiceUncompress EXPORTS Main;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT Trace;
IMPORT CardSeq;
IMPORT Debug;
IMPORT Params;
IMPORT UnsafeWriter;
IMPORT FS;
IMPORT TraceFile;
IMPORT FileWr;
IMPORT Wr;
IMPORT Time;
FROM Fmt IMPORT F, Int;

PROCEDURE Map(READONLY ia : ARRAY OF LONGREAL;
              iseq        : CardSeq.T;
              VAR      oa : ARRAY OF LONGREAL) =
  BEGIN
    FOR i := 0 TO iseq.size() - 1 DO
      oa[i] := ia[iseq.get(i)]
    END
  END Map;

CONST
  Usage = " [-minstep <minstep> ] <rootname>";
  
VAR
  pp := NEW(ParseParams.T).init(Stdio.stderr);
  minStep := 0.0d0;
  ibuff, obuff : REF ARRAY OF LONGREAL;

  root : TEXT;
  trace : Trace.T;
  active := NEW(CardSeq.T).init();
  
BEGIN
  TRY
    IF pp.keywordPresent("-minstep") THEN
      minStep := pp.getNextLongReal()
    END;

    pp.skipParsed();

    root := pp.getNext();

    pp.finish()
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command-line parameters\nUsage: " & Params.Get(0) & " " & Usage)
  END;

  TRY
    FS.DeleteFile(root & ".trace")
  EXCEPT
  ELSE
  END;

  trace := NEW(Trace.T).init(root);

  Debug.Out(F("steps %s", Int(trace.getSteps())));
  ibuff := NEW(REF ARRAY OF LONGREAL, trace.getSteps());

  VAR
    p : LONGREAL;
  BEGIN
    trace.getTimeData(ibuff^);
    p := ibuff[0];
    active.addhi(0);
    
    FOR i := 1 TO LAST(ibuff^) DO
      IF ibuff[i] >= p + minStep THEN
        p := ibuff[i];
        active.addhi(i)
      END
    END
  END;

  obuff := NEW(REF ARRAY OF LONGREAL, active.size());

  WITH header = TraceFile.Header { TraceFile.Version.Reordered,
                                   Time.Now(),
                                   trace.getNodes() },
       wr     = FileWr.Open(root & ".trace") DO

    TraceFile.WriteHeader(wr, header);

    FOR ni := 0 TO trace.getNodes() - 1 DO
      trace.getNodeData(ni, ibuff^);
      Map(ibuff^, active, obuff^);
      UnsafeWriter.WriteLRA(wr, obuff^)
    END;

    Wr.Close(wr)
  END
END SpiceUncompress.
