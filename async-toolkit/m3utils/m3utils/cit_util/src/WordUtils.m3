MODULE WordUtils;

PROCEDURE FromBits(READONLY bits: Bits): T =
  VAR
    acc := 0;
  BEGIN
    FOR i := 0 TO LAST(bits) DO
      acc := acc*2 + ORD(bits[i]);
    END;
    RETURN acc;
  END FromBits;

BEGIN
END WordUtils.
