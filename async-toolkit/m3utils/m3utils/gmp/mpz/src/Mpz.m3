UNSAFE MODULE Mpz;

IMPORT MpzRep;

IMPORT MpzP AS P;
IMPORT Word;
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
      FormatBase.Octal => 
    |
      FormatBase.Decimal => 
    |
      FormatBase.Hexadecimal => 
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
  BEGIN
  END FormatHexadecimal;

PROCEDURE FormatOctal(t : T) : TEXT =
  BEGIN
  END FormatOctal;

BEGIN END Mpz.
