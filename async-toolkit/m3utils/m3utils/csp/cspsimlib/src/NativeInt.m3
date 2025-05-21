MODULE NativeInt;
IMPORT Fmt AS FmtIntf;
IMPORT Mpz;
IMPORT Debug;

CONST doDebug = TRUE;
      
PROCEDURE Pow(b, x : T) : T =
  VAR
    r : T;
    result := One;
  BEGIN
    (* this follows the CSP semantics *)
    IF b = 1 OR x = 0 THEN RETURN 1 END;

    IF x < 0 THEN RETURN 0 END;

    WHILE x # 0 DO
      r := x MOD 2;
      x := x DIV 2;

      IF r # 0 THEN
        result := result * b
      END;
      b := b * b
    END;
    RETURN result
  END Pow;

PROCEDURE Hex(a : T) : TEXT =
  BEGIN
    RETURN FmtIntf.Int(a, base := 16)
  END Hex;

PROCEDURE ConvertDynamicInt(scratch, from : Mpz.T) : T =
  BEGIN
    Mpz.and(scratch, from, Mask);
    WITH res = Mpz.ToInteger(scratch) DO
      IF doDebug THEN
        Debug.Out(FmtIntf.F("NativeInt.ConvertDynamicInt %s -> %s -> %s",
                            Mpz.FormatDecimal(from),
                            Mpz.FormatDecimal(scratch),
                            FmtIntf.Int(res)))
      END;
      RETURN res
    END
  END ConvertDynamicInt;

VAR
  Mask := Mpz.NewInt(1);
BEGIN
  Mpz.LeftShift(Mask, Mask, BITSIZE(INTEGER));
  Mpz.sub_ui   (Mask, Mask, 1)
END NativeInt.

