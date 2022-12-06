MODULE TestMain EXPORTS Main;
IMPORT ArithCode;
IMPORT FreqTable;
IMPORT FileWr;
IMPORT ArithCoder;
IMPORT ArithCallback;
IMPORT FileRd;
IMPORT TextWr;
IMPORT Debug;
FROM Fmt IMPORT F;
IMPORT Wr;
IMPORT Rd;

CONST
  TestString = "";

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


  WITH encoder = arithCode.newEncoder(),
       rd      = FileRd.Open("mac.txt"),
       wr      = FileWr.Open("mac.out"),
       cb      = NEW(ArithCallback.Writer).init(wr) DO

    encoder.setCallback(cb);

    TRY
      LOOP
        WITH line = Rd.GetLine(rd) DO
          encoder.text(line);
          encoder.text("\n")
        END
      END
    EXCEPT
      Rd.EndOfFile =>      
      encoder.eof();
      Wr.Close(wr)
    END;
  END;
  
  WITH decoder = arithCode.newDecoder(),
       rd      = FileRd.Open("mac.out"),
       txtWr   = TextWr.New(),
       cb      = NEW(ArithCallback.Writer).init(txtWr) DO
    decoder.setCallback(cb);

    decoder.rdTillEof(rd);

    <*ASSERT txtWr # NIL*>
    
    Debug.Out(F("Decoded \"%s\"", TextWr.ToText(txtWr)))
  END;

END TestMain.
