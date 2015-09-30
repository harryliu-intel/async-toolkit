MODULE Main;
IMPORT Search;
FROM Search IMPORT Placement;

TYPE
  Checker = Search.Checker OBJECT 
  OVERRIDES
    makePlacement := MakePlacement;
    placementOK := PlacementOK;
    unmakePlacement := UnmakePlacement;
  END;

  Thunk = Search.Thunk OBJECT END;

PROCEDURE MakePlacement(c : Checker; READONLY new : Placement) : Thunk =
  BEGIN 
    RETURN NIL 
  END MakePlacement;

VAR

BEGIN
END Main.

