MODULE PowerAnal EXPORTS Main;
IMPORT Trace;
IMPORT ParseParams;
IMPORT Debug;
IMPORT Stdio;
IMPORT Params;
IMPORT Pathname;
IMPORT RegEx;
IMPORT Text;
FROM Fmt IMPORT F, Int;
IMPORT RefSeq;
IMPORT TraceInterpolator;

CONST Usage   = "";
      Verbose = FALSE;

TYPE
  Measurement = OBJECT
    nm, pfx, sfx : TEXT;
    E            : LONGREAL;
  END;
      
PROCEDURE DoReports(tr : Trace.T) =
  <*FATAL RegEx.Error*>
  VAR
    allNames := tr.allNames();
    iter     := allNames.iterate();
    regex    := RegEx.Compile("eallc([^()]*)$");

    n        : TEXT;

    matches  := NEW(RefSeq.T).init();
    idx      : Trace.NodeId;
    scratch  := NEW(REF REF ARRAY OF LONGREAL);
  BEGIN
    WHILE iter.next(n) DO
      WITH start = RegEx.Execute(regex, n) DO
        IF start # -1 THEN
          WITH suffix = Text.Sub(n, start       ),
               prefix = Text.Sub(n,     0, start) DO
            IF Verbose THEN
              Debug.Out(F("Found match %s , %s", prefix, suffix))
            END;

            matches.addhi(NEW(Measurement,
                              nm := n, pfx := prefix, sfx := suffix))
          END
        END
      END
    END;

    Debug.Out(F("%s matches", Int(matches.size())));

    FOR i := 0 TO matches.size() - 1 DO
      WITH m     = NARROW(matches.get(i), Measurement),
           found = tr.getNodeIdx(m.nm, idx),
           int   = NEW(TraceInterpolator.T).init(tr, idx, scratch),
           loE   = int.eval(frTime),
           hiE   = int.eval(toTime) DO
        <*ASSERT found*>
        m.E := hiE - loE
      END
    END
  END DoReports;

PROCEDURE ppLR(kw : TEXT) : LONGREAL =
  BEGIN
    IF NOT pp.keywordPresent(kw) THEN
      Debug.Error("Must specify " & kw)
    END;
    RETURN pp.getNextLongReal()
  END ppLR;
  
VAR
  pp            := NEW(ParseParams.T).init(Stdio.stderr);
  root           : Pathname.T;
  frTime, toTime : LONGREAL;
  freq           : LONGREAL;
BEGIN
  TRY
    IF pp.keywordPresent("-root") THEN    
      root := pp.getNext()
    END;

    frTime := ppLR("-from");
    toTime := ppLR("-to");
    freq   := ppLR("-freq");
    
    pp.skipParsed();
    pp.finish()
  EXCEPT
    ParseParams.Error =>
    Debug.Error("Can't parse command-line parameters\nUsage: " & Params.Get(0) & " " & Usage)
  END;

  VAR
    trace := NEW(Trace.T).init(root);
  BEGIN
    DoReports(trace)
  END
END PowerAnal.
