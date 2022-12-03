MODULE TestMain EXPORTS Main;
IMPORT ArithCode;
IMPORT FreqTable;
IMPORT FileWr;
IMPORT ArithCoder;
IMPORT ArithCallback;

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
    INC(ft[i])
  END;
  INC(ft[LAST(ft)]);

  arithCode := NEW(ArithCode.T).init(ft);

  WITH encoder = arithCode.newEncoder(),
       wr      = FileWr.Open("test.out"),
       cb      = NEW(ArithCallback.T) DO
    encoder.setCallback(cb);

    WITH str = "The quick brown fox jumped over the lazy dogs!\000\200" DO
      encoder.text(str);
      encoder.eof()
    END
  END
  
END TestMain.
