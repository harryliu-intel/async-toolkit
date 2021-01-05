MODULE PicOverlay;
IMPORT Pic;

REVEAL
  T = Public BRANDED Brand OBJECT
    over, under : Pic.T;
  OVERRIDES
    init := Init;
  END;

PROCEDURE Init(t : T; over, under : Pic.T) : T =
  BEGIN
    t.over := over;
    t.under := under;
    RETURN t
  END Init;

BEGIN END PicOverlay.
