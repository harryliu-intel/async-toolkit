MODULE MpzSafe EXPORTS Mpz;
IMPORT MpzRep;
IMPORT Word;
IMPORT Debug;
FROM Fmt IMPORT F;


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
  
PROCEDURE InitScan(txt : TEXT; base : CARDINAL) : T =
  VAR
    res := NEW(T);
  BEGIN
    EVAL init_set_str(res, txt, base);
    RETURN res
  END InitScan;

PROCEDURE pow(p, b, x : T) =
  VAR
    xui := get_ui(x);
  BEGIN
    <*ASSERT fits_ulong_p(x) = 1*>
    pow_ui(p, b, xui);
  END pow;

PROCEDURE ToInteger(t : T) : INTEGER =
  BEGIN
    IF cmp(t, MaxInt) > 0 OR cmp(t, MinInt) < 0 THEN
      Debug.Error(F("Mpz.ToInteger : not an INTEGER : %s [%s]",
                    FormatDecimal(t), FormatBased(t, 16)))
    END;
    RETURN get_si(t)
  END ToInteger;

PROCEDURE ToWord(t : T) : Word.T =
  BEGIN
    IF cmp(t, MaxWord) > 0 OR cmp(t, Zero) < 0 THEN
      Debug.Error(F("Mpz.ToWord : not a Word.T : %s [%s]",
                    FormatDecimal(t), FormatBased(t, 16)))
    END;
    RETURN get_ui(t)
  END ToWord;

VAR
  MinInt  := NEW(T);
  MaxInt  := NEW(T);
  Zero    := NEW(T);
  One     := NEW(T);
  Two     := NEW(T);
  MaxWord := NEW(T);
BEGIN
  init_set_si(MinInt , FIRST(INTEGER));
  init_set_si(MaxInt , LAST(INTEGER));
  init_set_si(Zero   , 0);
  init_set_si(One    , 1);
  init_set_si(Two    , 2);
  init_set_si(MaxWord, 2);
  pow_ui     (MaxWord, MaxWord, 64);
  sub_ui     (MaxWord, MaxWord,  1);
END MpzSafe.
