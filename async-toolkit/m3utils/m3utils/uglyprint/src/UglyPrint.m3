MODULE UglyPrint EXPORTS Main;
IMPORT Rd, Wr, Text;
IMPORT Stdio;
IMPORT Thread;

<*FATAL Thread.Alerted, Rd.Failure, Wr.Failure*>

CONST WhiteSpace = SET OF CHAR { ' ', '\t' };

PROCEDURE Push() = 
  BEGIN
    Wr.PutText(wr, buff);
    Wr.PutChar(wr, '\n');
    buff := ""
  END Push;

PROCEDURE StripSpace(line : TEXT) : TEXT =
  BEGIN
    WHILE Text.Length(line) > 0 AND 
          Text.GetChar(line, Text.Length(line)-1) IN WhiteSpace DO
      line := Text.Sub(line, 0, Text.Length(line)-1)
    END;
    RETURN line
  END StripSpace;

PROCEDURE EndLine(line : TEXT) : BOOLEAN =
  CONST Terms = ARRAY OF TEXT { ";" , "endmodule" };
  VAR 
    len := Text.Length(line);
  BEGIN
    FOR i := FIRST(Terms) TO LAST(Terms) DO
      WITH t  = Terms[i],
           tl = Text.Length(t) DO
        IF len >= tl AND Text.Equal(Text.Sub(line, len-tl, tl), t) THEN
          RETURN TRUE
        END
      END
    END;
    RETURN FALSE
  END EndLine;

VAR
  buff := "";
  wr := Stdio.stdout;
  rd := Stdio.stdin;
BEGIN
  TRY
    LOOP
      WITH line = StripSpace(Rd.GetLine(rd)) DO
        buff := buff & line;
        IF EndLine(line) THEN Push() ELSE buff := buff & " " END
      END
    END
  EXCEPT
    Rd.EndOfFile =>
  END;
  Push();
  Wr.Flush(wr)
END UglyPrint.
