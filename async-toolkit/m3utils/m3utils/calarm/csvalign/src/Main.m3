(* $Id$ *)

MODULE Main;

IMPORT ParseParams, Stdio;
IMPORT Wr, Text, FileRd, TextSeq, RefSeq;
IMPORT Debug, Rd;
IMPORT Thread;
IMPORT OSError;

<*FATAL Thread.Alerted, Wr.Failure, OSError.E, Rd.Failure*>

VAR
  rd      := Stdio.stdin;
  wr      := Stdio.stdout;
  lines   := NEW(RefSeq.T).init();
  maxflds := 0;
  sep     := " ";

PROCEDURE MaxWidth(s : TextSeq.T; VAR w : ARRAY OF CARDINAL) =
  BEGIN
    FOR i := 0 TO s.size()-1 DO
      w[i] := MAX(w[i], Text.Length(s.get(i)))
    END
  END MaxWidth;

PROCEDURE DumpLine(s : TextSeq.T; READONLY w : ARRAY OF CARDINAL) = 
  BEGIN
    FOR i := 0 TO s.size()-1 DO
      WITH t = s.get(i) DO
        Wr.PutText(wr, t);
        FOR i := 0 TO w[i] - Text.Length(t) DO
          Wr.PutChar(wr, ' ')
        END;
        IF i # s.size()-1 THEN
          Wr.PutText(wr, sep)
        END
      END;
    END
  END DumpLine;

BEGIN
  TRY 
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      IF pp.keywordPresent("-sep") OR pp.keywordPresent("-separator") THEN
        sep := pp.getNext()
      END;

      IF pp.keywordPresent("-i") THEN
        WITH fn = pp.getNext() DO
          rd := FileRd.Open(fn)
        END
      END;

      pp.skipParsed(); pp.finish()
    END;
  EXCEPT
    ParseParams.Error => Debug.Error("Couldn't parse cmd-line params")
  END;

  TRY
    LOOP
      VAR
        line := Rd.GetLine(rd);
        len := Text.Length(line);
        seq := NEW(TextSeq.T).init();
        p := 0;
      BEGIN
        FOR i := 0 TO len-1 DO
          WITH c = Text.GetChar(line,i) DO
            (* should be careful with quotation marks etc *)
            IF c = ',' THEN
              seq.addhi(Text.Sub(line, p, i-p));
              p := i+1
            END
          END
        END;
        maxflds := MAX(maxflds, seq.size());
        lines.addhi(seq)
      END
    END
  EXCEPT
    Rd.EndOfFile =>
    TRY Rd.Close(rd) EXCEPT ELSE END
  END;


  VAR
    w := NEW(REF ARRAY OF CARDINAL, maxflds);
  BEGIN
    FOR i := FIRST(w^) TO LAST(w^) DO
      w[i] := 0
    END;

    FOR i := 0 TO lines.size()-1 DO
      MaxWidth(lines.get(i), w^)
    END;

    FOR i := 0 TO lines.size()-1 DO
      DumpLine(lines.get(i), w^);
      Wr.PutChar(wr, '\n')
    END
  END

END Main.
