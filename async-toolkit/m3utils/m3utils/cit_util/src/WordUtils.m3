MODULE WordUtils;

IMPORT Wr, Fmt;
FROM Stdio IMPORT stderr;

PROCEDURE FromBits(READONLY bits: Bits): T =
  VAR
    acc := 0;
  BEGIN
    FOR i := 0 TO LAST(bits)  DO
      acc := acc + ORD(bits[i]) *i;
    END;
    RETURN acc;
  END FromBits;

BEGIN
END WordUtils.
