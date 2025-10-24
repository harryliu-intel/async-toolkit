(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE TestMain EXPORTS Main;
IMPORT ArithCode;
IMPORT FreqTable;
IMPORT FileWr;
IMPORT ArithCoder;
IMPORT ArithCallback;
IMPORT FileRd;
IMPORT TextWr;
IMPORT Debug;
FROM Fmt IMPORT F, Bool, Int, Pad;
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

PROCEDURE FreqEnDec(ifn, ofn : Pathname.T; tag : TEXT; print := TRUE) =
  VAR
    frd := FileRd.Open(ifn);
    ft   : FreqTable.T;
    code : ArithCode.T;
  BEGIN

    ft := FreqTable.FromRd(frd);
    Rd.Close(frd);

    WrFreqTableC(tag & "_freqs.h", tag & "_freqs", ft);
    
    code := NEW(ArithCode.T).init(ft);
    EnDec(code, ifn, ofn, print)
  END FreqEnDec;

PROCEDURE WrFreqTableC(pn : Pathname.T; nm : TEXT; READONLY tbl : FreqTable.T) =
  VAR
    wr := FileWr.Open(pn);
  BEGIN
    Wr.PutText(wr, F("static const FreqTable_t %s = {\n", nm));

    FOR i := FIRST(FreqTable.T) TO LAST(FreqTable.T) DO
      IF    i MOD 64 = 0 THEN
        Wr.PutText(wr, "\n\n  ")
      ELSIF i MOD 16 = 0 THEN
        Wr.PutText(wr, "\n  ")
      ELSIF i MOD  4 = 0 THEN
        Wr.PutText(wr, "  ")
      END;
      Wr.PutText(wr, Pad(Int(tbl[i]), 6));

      IF i # LAST(FreqTable.T) THEN
        Wr.PutChar(wr, ',')
      END
    END;

    Wr.PutText(wr, "\n    };\n");
    
    Wr.Close(wr)
  END WrFreqTableC;
  
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

  WrFreqTableC("etaoin.h", "etaoin_freqs", ft);
  
  arithCode := NEW(ArithCode.T).init(ft);

  IF TRUE THEN
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
  FreqEnDec("mac.txt", "mac.f_out", "mac");
  FreqEnDec("108091.z", "108091.z.f_out", "108091");
  FreqEnDec("108091_5ps.z", "108091_5ps.z.f_out", "108091_5ps", FALSE);
  
END TestMain.
