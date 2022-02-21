MODULE LibertyHead;
IMPORT LibertyComponent;
IMPORT LibertyParamList;

REVEAL
  T = LibertyComponent.T BRANDED Brand OBJECT
    ident  : TEXT;
    params : LibertyParamList.T;
  OVERRIDES
    format := Format;
  END;

PROCEDURE Format(t : T) : TEXT =
  BEGIN
  END Format;

PROCEDURE New(ident : TEXT; params : LibertyParamList.T) : T =
  BEGIN
    RETURN NEW(T, ident := ident, params := params)
  END New;

BEGIN END LibertyHead.
