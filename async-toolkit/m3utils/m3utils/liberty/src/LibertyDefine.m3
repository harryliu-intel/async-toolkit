MODULE LibertyDefine;
IMPORT LibertyComponent;

REVEAL
  T = LibertyComponent.T BRANDED Brand OBJECT
  OVERRIDES
    format := Format;
  END;

PROCEDURE Format(t : T) : TEXT =
  BEGIN
  END Format;

BEGIN END LibertyDefine.

  
