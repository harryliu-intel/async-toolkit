(* $Id: WordUtils.m3,v 1.4 2004/07/14 01:48:53 mika Exp $ *)
MODULE WordUtils;

PROCEDURE FromRails(READONLY rails: Rails): T =
  VAR
    acc := 0;
  BEGIN
    FOR i := 0 TO LAST(rails)  DO
      acc := acc + ORD(rails[i]) *i;
    END;
    RETURN acc;
  END FromRails;

BEGIN
END WordUtils.
