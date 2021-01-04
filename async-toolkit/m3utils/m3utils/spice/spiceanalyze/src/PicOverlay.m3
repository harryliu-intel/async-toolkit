MODULE PicOverlay;
IMPORT Pic;

REVEAL
  T = Public BRANDED Brand OBJECT
    over : Pic.T;
  OVERRIDES
    init := Init;
  END;

PROCEDURE Init(t : T; over : Pic.T) : T =
  BEGIN
    t.over := over;
    RETURN t
  END Init;

BEGIN END PicOverlay.
