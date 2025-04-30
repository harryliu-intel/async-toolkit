MODULE NativeInt;
IMPORT Fmt AS FmtIntf;

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
  
BEGIN END NativeInt.

