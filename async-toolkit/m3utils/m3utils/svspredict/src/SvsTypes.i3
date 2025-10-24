INTERFACE SvsTypes;

TYPE
  CornerData = RECORD
    vtiming, vpower, sigma : LONGREAL;
  END;

PROCEDURE FmtCornerData(READONLY cd : CornerData) : TEXT;

CONST Brand = "SvsTypes";

END SvsTypes.
