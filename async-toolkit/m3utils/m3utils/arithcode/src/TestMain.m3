MODULE TestMain EXPORTS Main;
IMPORT ArithCode;
IMPORT FreqTable;
IMPORT FileWr;
IMPORT ArithCoder;
IMPORT ArithCallback;
IMPORT FileRd;
IMPORT TextWr;
IMPORT Debug;
FROM Fmt IMPORT F, Bool;
IMPORT Wr;
IMPORT Rd;
IMPORT Pathname;
IMPORT Text;

CONST
  TestString = "";

PROCEDURE EnDec(code : ArithCode.T; ifn, ofn : Pathname.T; print := TRUE) =
  VAR
    mem : TEXT;
  BEGIN
    WITH encoder = code.newEncoder(),
         rd      = FileRd.Open(ifn),
         wr      = FileWr.Open(ofn),
         cb      = NEW(ArithCallback.Writer).init(wr),
         memWr   = TextWr.New() DO
      
      encoder.setCallback(cb);
      
      TRY
        LOOP
          WITH c = Rd.GetChar(rd) DO
            encoder.char(c);
            Wr.PutChar(memWr, c)
          END
        END
      EXCEPT
        Rd.EndOfFile =>      
        encoder.eof();
        Wr.Close(wr);
        mem := TextWr.ToText(memWr)
      END;
    END;
  
    WITH decoder = code.newDecoder(),
         rd      = FileRd.Open(ofn),
         txtWr   = TextWr.New(),
         cb      = NEW(ArithCallback.Writer).init(txtWr) DO
      decoder.setCallback(cb);
      
      decoder.rdTillEof(rd);
      
      <*ASSERT txtWr # NIL*>

      WITH ot = TextWr.ToText(txtWr) DO
        IF print THEN
          Debug.Out(F("Decoded \"%s\"", ot))
        END;
        WITH eq = Text.Equal(ot, mem) DO
          Debug.Out(F("(input = output) = %s", Bool(eq)));
          <*ASSERT eq*>
        END
      END
    END
    
  END EnDec;

PROCEDURE FreqEnDec(ifn, ofn : Pathname.T; print := TRUE) =
  VAR
    frd := FileRd.Open(ifn);
    ft : FreqTable.T;
    code : ArithCode.T;
  BEGIN

    ft := FreqTable.FromRd(frd);
    Rd.Close(frd);

    code := NEW(ArithCode.T).init(ft);
    EnDec(code, ifn, ofn, print)
  END FreqEnDec;
  
VAR
  ft := FreqTable.T { 0, .. };
  arithCode : ArithCode.T;
BEGIN
  (* set up some basic encoding table *)
  FOR i := 0 TO 127 DO
    INC(ft[i])
  END;
  INC(ft[0]);
  FOR i := ORD(' ') TO ORD('z') DO
    INC(ft[i], 2)
  END;
  FOR i := ORD('a') TO ORD('z') DO
    (* mostly lower case *)
    INC(ft[i], 3)
  END;
  WITH arr = ARRAY OF CHAR { 'e', 't', 'a', 'o', 'i', 'n' } DO
    FOR j := FIRST(arr) TO LAST(arr) DO
      INC(ft[ORD(arr[j])], 3)
    END
  END;
  INC(ft[ORD(' ')], 3);
  INC(ft[LAST(ft)], 2);

  arithCode := NEW(ArithCode.T).init(ft);

  IF FALSE THEN
    WITH encoder = arithCode.newEncoder(),
         wr      = FileWr.Open("test.out"),
         cb      = NEW(ArithCallback.Writer).init(wr) DO
      encoder.setCallback(cb);
      
      WITH str = "The quick brown fox jumped over the lazy dogs!" DO
        encoder.text(str & " " & str);
        encoder.eof();
        Wr.Close(wr)
      END
    END;
    
    WITH decoder = arithCode.newDecoder(),
         rd      = FileRd.Open("test.out"),
         txtWr   = TextWr.New(),
         cb      = NEW(ArithCallback.Writer).init(txtWr) DO
      decoder.setCallback(cb);
      
      decoder.rdTillEof(rd);
      
      <*ASSERT txtWr # NIL*>
      
      Debug.Out(F("Decoded \"%s\"", TextWr.ToText(txtWr)))
    END
  END;

  EnDec(arithCode, "mac.txt", "mac.out");
  FreqEnDec("mac.txt", "mac.f_out");
  FreqEnDec("108091.z", "108091.z.f_out");
  FreqEnDec("108091_5ps.z", "108091_5ps.z.f_out", FALSE);
  
END TestMain.
