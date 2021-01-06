MODULE PicComponent;
IMPORT PicSegments;
IMPORT PicOverlay;
IMPORT CktElement;

REVEAL
  T = Public BRANDED Brand OBJECT
    over, under : PicSegments.T;
    neighbors   : ARRAY Step OF ARRAY Step OF T;
    obj         : CktElement.T;
  OVERRIDES
    init := Init;
    setNeighbor := SetNeighbor;
  END;

PROCEDURE Init(t : T; obj : CktElement.T) : T =
  VAR
  BEGIN
    t.over     := NEW(PicSegments.T).init();
    t.under    := NEW(PicSegments.T).init();
    t.obj      := obj;
    FOR i := FIRST(t.neighbors) TO LAST(t.neighbors) DO
      FOR j := FIRST(t.neighbors[i]) TO LAST(t.neighbors[i]) DO
        t.neighbors[i, j] := NIL
      END
    END;
    
    RETURN PicOverlay.T.init(t,
                             over  := t.over,
                             under := t.under);
  END Init;

PROCEDURE SetNeighbor(t : T; dx, dy : Step; n : T) =
  BEGIN
    t.neighbors[dx, dy] := n
  END SetNeighbor;

BEGIN END PicComponent.
