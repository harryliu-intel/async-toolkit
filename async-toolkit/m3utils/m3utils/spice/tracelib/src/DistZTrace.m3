MODULE DistZTrace;
IMPORT Wr;
IMPORT UnsafeWriter;
IMPORT SpiceCompress;
IMPORT TextWr;
FROM Fmt IMPORT Int, F;
IMPORT Debug;
IMPORT Text;
IMPORT Thread;
IMPORT Matrix;
IMPORT TripleRefTbl;
IMPORT ArithConstants;
IMPORT ArithCode;
IMPORT ArithCallback;

CONST TE = Text.Equal;

PROCEDURE DoArithCompress(of : TEXT;
                          VAR codeIdx : ArithConstants.CodeIdx) : TEXT =
  VAR
    enTxt : TEXT;
  BEGIN
    codeIdx := 1;

    WITH ft      = ArithConstants.CodeBook[codeIdx],
         code    = NEW(ArithCode.T).init(ft),
         encoder = code.newEncoder(),
         enWr    = TextWr.New(),
         enCb    = NEW(ArithCallback.Writer).init(enWr),

         (* verify code *)
         decoder = code.newDecoder(),
         deWr    = TextWr.New(),
         deCb    = NEW(ArithCallback.Writer).init(deWr)
     DO
      encoder.setCallback(enCb);
      encoder.text(of);
      encoder.eof();

      enTxt := TextWr.ToText(enWr);
      
      decoder.setCallback(deCb);
      decoder.text(enTxt);
      decoder.eof();

      WITH deTxt = TextWr.ToText(deWr) DO
        IF NOT TE(of, deTxt) THEN
          VAR
            msg : TEXT;
            ofLen := Text.Length(of);
            deLen := Text.Length(deTxt);
          BEGIN
            IF ofLen # deLen THEN
              msg := F("length mismatch of %s # de %s", Int(ofLen), Int(deLen))
            ELSE
              FOR i := 0 TO ofLen - 1 DO
                WITH ofChar = Text.GetChar(of, i),
                     deChar = Text.GetChar(deTxt, i) DO
                  IF ofChar # deChar THEN
                    msg := F("mismatch @ i=%s : of[i]='%s' # de[i]='%s'",
                             Int(i),
                             Text.FromChar(ofChar), Text.FromChar(deChar))
                  END
                END
              END
            END;
            Debug.Error("??? DoArithCompress verify error, " & msg)
          END
        END
      END
    END;

    RETURN enTxt
  END DoArithCompress;

PROCEDURE WriteOut(wr            : Wr.T;

                   VAR a         : ARRAY OF LONGREAL;
                   (* a will be normalized in place *)
                   
                   nodeid        : CARDINAL;
                   (* nodeid in trace file whither data is destined *)
                   
                   doDump        : BOOLEAN;
                   
                   relPrec       : LONGREAL;
                   
                   doAllDumps    : BOOLEAN;

                   noArith       : BOOLEAN)
  RAISES { Thread.Alerted, Wr.Failure, Matrix.Singular } =
  TYPE
    ALR1 = ARRAY [ 0..0 ] OF LONGREAL;
  VAR
          code     : ArithConstants.CodeIdx;
      finalTxt : TEXT;
      finalLen : CARDINAL;
                   norm          : SpiceCompress.Norm;

  BEGIN
    WITH z      = NEW(REF ARRAY OF LONGREAL, NUMBER(a)),
           textWr = NEW(TextWr.T).init() DO

          SpiceCompress.CompressArray("zdebug",
                                      a,
                                      z^,
                                      relPrec,
                                      doAllDumps,
                                      textWr,
                                      norm,
                                      mem    := NEW(TripleRefTbl.Default).init(),
                                      doDump := doDump);
        (* we now have the polynomially compressed in textWr *)
        
        WITH txt = TextWr.ToText(textWr),
             len = Text.Length(txt) DO

          (* txt is the polynomially compressed waveform *)

          IF noArith THEN
            code     := ArithConstants.ZeroCode;
            finalTxt := txt;
            finalLen := len
          ELSE
            finalTxt := DoArithCompress(txt, code);
            finalLen := Text.Length(finalTxt)
          END;

          Debug.Out(F("%s timesteps, compressed size %s bytes, coded size %s",
                      Int(NUMBER(a)),
                      Int(len),
                      Int(finalLen)));





    Wr.PutText           (wr, "ZZZ\n"); (* advertise compressed data *)
    Wr.PutChar           (wr, 'x');     (* tag (unused) *)
    UnsafeWriter.WriteI  (wr, nodeid);
    UnsafeWriter.WriteI  (wr, finalLen + 1 + 2 * 4);
    UnsafeWriter.WriteLRA(wr, ALR1 { norm.min } );
    UnsafeWriter.WriteLRA(wr, ALR1 { norm.max } );
    Wr.PutChar           (wr, VAL(code, CHAR));
    Wr.PutText           (wr, finalTxt);
        END
      END
  END WriteOut;
  
BEGIN END DistZTrace.
