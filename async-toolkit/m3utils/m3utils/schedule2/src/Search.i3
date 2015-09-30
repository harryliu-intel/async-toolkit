INTERFACE Search;
IMPORT CardSeq;

TYPE
  Thunk = BRANDED Brand & " Thunk" OBJECT END;

  Checker = OBJECT METHODS
    makePlacement(READONLY new : Placement) : Thunk;
    placementOK(READONLY pl : Placement) : BOOLEAN;
    unmakePlacement(READONLY pl : Placement; thunk : Thunk);
  END;
    
  Placement = ARRAY OF SinglePlacement;
  
  RevPlacement = ARRAY OF CardSeq.T;
  
  SinglePlacement = [-1..LAST(CARDINAL)];

CONST Brand = "Search";

END Search.
