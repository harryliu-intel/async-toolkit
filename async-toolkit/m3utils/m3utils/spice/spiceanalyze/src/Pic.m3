MODULE Pic;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    init := Init;
  END;

PROCEDURE Init(t : T) : T =
  BEGIN
    RETURN t
  END Init;

BEGIN END Pic.
