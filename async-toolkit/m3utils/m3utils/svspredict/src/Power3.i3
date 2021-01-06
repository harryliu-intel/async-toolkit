INTERFACE Power3;

TYPE
  T = RECORD
    dynP, lkgP, totP : LONGREAL;
  END;

PROCEDURE DebugFmt(READONLY a : T) : TEXT;

CONST Brand = "Power3";

END Power3.
