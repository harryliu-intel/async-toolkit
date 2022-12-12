MODULE TempDataRep;
IMPORT Debug;
IMPORT Text;
FROM Fmt IMPORT F, Int;
IMPORT TextRd;
IMPORT TextWr;
IMPORT ArithCode;
IMPORT ArithConstants;
IMPORT ArithCallback;
IMPORT UnsafeReader;
IMPORT Rd;
IMPORT Thread;
IMPORT SpiceCompress;
IMPORT FileWr;
IMPORT Wr;

PROCEDURE ReadFromTemp(tempData    : TEXT;
                       VAR into    : T) =
  <*FATAL Thread.Alerted, Rd.EndOfFile, Rd.Failure*>
  VAR
    rd := TextRd.New(tempData);

    finalLen := Text.Length(tempData) - 9;
    finalData := NEW(REF ARRAY OF CHAR, finalLen);
  BEGIN
    (* see Fsdb.m3 / Fsdb.DoCompressedReceive *)
    into.norm.min := UnsafeReader.ReadLR(rd); 
    into.norm.max := UnsafeReader.ReadLR(rd);

    (* see Main.m3<spicestream> *)
    into.code := ORD(Rd.GetChar(rd));

    WITH bytes = Rd.GetSub(rd, finalData^) DO
      <*ASSERT bytes = finalLen*>
      into.finalData := Text.FromChars(finalData^)
    END
  END ReadFromTemp;

PROCEDURE Reconstruct(READONLY t : T; VAR a : ARRAY OF LONGREAL) =
  VAR
    deTxt   : TEXT;
    fiLen   := Text.Length(t.finalData);
  BEGIN
    Debug.Out(F("TempDataRep.Reconstruct : code %s fiLen %s",
                Int(t.code), Int(fiLen)));
    
    IF t.code # ArithConstants.ZeroCode THEN
      WITH     ft      = ArithConstants.CodeBook[t.code],
               code    = NEW(ArithCode.T).init(ft),
               decoder = code.newDecoder(),
               deWr    = TextWr.New(),
               deCb    = NEW(ArithCallback.Writer).init(deWr) DO
        (* see Main.m3<spicestream>  / Main.DoArithCompress *)
        decoder.setCallback(deCb);
        decoder.text(t.finalData);
        decoder.eof();
        deTxt := TextWr.ToText(deWr)
      END
    ELSE
      deTxt := t.finalData
    END;

    WITH fWr = FileWr.Open("reconstruct_raw") DO
      Wr.PutText(fWr, deTxt);
      Wr.Close(fWr)
    END;
    
    WITH deLen = Text.Length(deTxt),
         deRd  = TextRd.New(deTxt) DO
      Debug.Out(F("Arithmetic decode complete, %s -> %s bytes",
                  Int(fiLen), Int(deLen)));
      SpiceCompress.DecompressArray(deRd, a)
    END
  END Reconstruct;

BEGIN END TempDataRep.
