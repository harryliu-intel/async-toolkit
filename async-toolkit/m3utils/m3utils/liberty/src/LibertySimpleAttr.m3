MODULE LibertySimpleAttr;
IMPORT LibertyHead;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    format := Format;
  END;

PROCEDURE Format(t : T) : TEXT =
  BEGIN
  END Format;

BEGIN END LibertySimpleAttr.
