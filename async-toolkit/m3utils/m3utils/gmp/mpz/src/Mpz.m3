UNSAFE MODULE Mpz;

IMPORT MpzRep;

IMPORT MpzP AS P;
IMPORT WeakRef;
IMPORT M3toC;
IMPORT Word;

PROCEDURE New() : T =
  VAR
    res := NEW(T);
  BEGIN
    P.c_init(LOOPHOLE(ADR(res.val), P.MpzPtrT));
    EVAL WeakRef.FromRef(res, CleanUp);
    RETURN res
  END New;

PROCEDURE CleanUp(<*UNUSED*>READONLY w : WeakRef.T; r : REFANY) =
  BEGIN
    WITH this = NARROW(r, T) DO
      P.c_clear(ADR(this.val))
    END
  END CleanUp;

PROCEDURE Format(t : T; base := FormatBase.Decimal) : TEXT =
  BEGIN
    CASE base OF
      FormatBase.Binary => <*ASSERT FALSE*>
    |
      FormatBase.Octal =>  RETURN FormatOctal(t)
    |
      FormatBase.Decimal => RETURN FormatDecimal(t)
    |
      FormatBase.Hexadecimal => RETURN FormatHexadecimal(t)
    END     
  END Format;
  
PROCEDURE FormatDecimal(t : T) : TEXT =
  VAR
    cs := P.format_decimal(ADR(t.val));
  BEGIN
    TRY
      RETURN M3toC.CopyStoT(cs)
    FINALLY
      P.free_formatted(cs)
    END
  END FormatDecimal;

PROCEDURE FormatHexadecimal(t : T) : TEXT =
  VAR
    cs := P.format_hexadecimal(ADR(t.val));
  BEGIN
    TRY
      RETURN M3toC.CopyStoT(cs)
    FINALLY
      P.free_formatted(cs)
    END
  END FormatHexadecimal;

PROCEDURE FormatOctal(t : T) : TEXT =
  VAR
    cs := P.format_octal(ADR(t.val));
  BEGIN
    TRY
      RETURN M3toC.CopyStoT(cs)
    FINALLY
      P.free_formatted(cs)
    END
  END FormatOctal;

PROCEDURE Import(t : T; READONLY data : ARRAY OF Word.T) =
  BEGIN
    P.import(ADR(t.val), NUMBER(data), -1, BYTESIZE(Word.T), 0, 0, ADR(data[0]))
  END Import;

PROCEDURE Export(VAR data : ARRAY OF Word.T; t : T) =
  CONST
    numb = BITSIZE(Word.T);
  BEGIN
    FOR i := FIRST(data) TO LAST(data) DO
      data[i] := 0
    END;
    
    WITH count = (P.c_sizeinbase(ADR(t), 2) + numb - 1) DIV numb DO
      IF count <= NUMBER(data) THEN
        VAR
          ndata := NEW(REF ARRAY OF Word.T, count);
        BEGIN
          P.export(ADR(ndata[0]), NIL, -1, BYTESIZE(Word.T), 0, 0, ADR(t.val));
          data := SUBARRAY(ndata^, 0, NUMBER(data))
        END
      ELSE
        P.export(ADR(data[0]), NIL, -1, BYTESIZE(Word.T), 0, 0, ADR(t.val))
      END
    END
  END Export;

BEGIN END Mpz.
