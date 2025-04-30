UNSAFE MODULE Mpz;

IMPORT MpzRep;

IMPORT MpzP AS P;
IMPORT WeakRef;
IMPORT M3toC;

PROCEDURE New() : T =
  VAR
    res := NEW(T);
  BEGIN
    P.c_mpz_init(LOOPHOLE(ADR(res.val), P.MpzPtrT));
    EVAL WeakRef.FromRef(res, CleanUp);
    RETURN res
  END New;

PROCEDURE CleanUp(<*UNUSED*>READONLY w : WeakRef.T; r : REFANY) =
  BEGIN
    WITH this = NARROW(r, T) DO
      P.c_mpz_clear(ADR(this.val))
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
    cs := P.mpz_format_decimal(ADR(t.val));
  BEGIN
    TRY
      RETURN M3toC.CopyStoT(cs)
    FINALLY
      P.mpz_free_formatted(cs)
    END
  END FormatDecimal;

PROCEDURE FormatHexadecimal(t : T) : TEXT =
  VAR
    cs := P.mpz_format_hexadecimal(ADR(t.val));
  BEGIN
    TRY
      RETURN M3toC.CopyStoT(cs)
    FINALLY
      P.mpz_free_formatted(cs)
    END
  END FormatHexadecimal;

PROCEDURE FormatOctal(t : T) : TEXT =
  VAR
    cs := P.mpz_format_octal(ADR(t.val));
  BEGIN
    TRY
      RETURN M3toC.CopyStoT(cs)
    FINALLY
      P.mpz_free_formatted(cs)
    END
  END FormatOctal;

BEGIN END Mpz.
