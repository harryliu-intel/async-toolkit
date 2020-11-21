MODULE Main;
IMPORT Rd, Wr;
IMPORT FileRd;
IMPORT Params;
FROM Fmt IMPORT F;
IMPORT FileWr;
IMPORT Text;

VAR rd  := FileRd.Open(Params.Get(1));
    oin := Params.Get(2);
    wr  := FileWr.Open(oin & ".i3");
    
BEGIN
  Wr.PutText(wr, "INTERFACE ");
  Wr.PutText(wr, oin);
  Wr.PutText(wr, F("\n\nCONST Brand = \"%s\";\n\n", oin));
  
  TRY
    LOOP
      VAR
        line := Rd.GetLine(rd);
        len  := Text.Length(line);
      BEGIN
        Wr.PutText(wr, "CONST ");
        Wr.PutText(wr, line);
        Wr.PutText(wr, "kw := ARRAY OF CHAR { ");
        FOR i := 0 TO len - 1 DO
          Wr.PutText(wr, F("'%s'", Text.Sub(line, i, 1)));
          IF i # len - 1 THEN
            Wr.PutText(wr, ", ")
          END
        END;
        Wr.PutText(wr, " };\n")
      END
    END
  EXCEPT
    Rd.EndOfFile =>
    Wr.PutText(wr, F("\nEND %s.\n", oin));
    Rd.Close(rd);
    Wr.Close(wr)
  END
END Main.
