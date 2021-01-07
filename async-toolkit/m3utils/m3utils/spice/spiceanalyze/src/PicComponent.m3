MODULE PicComponent;
IMPORT PicSegments;
IMPORT PicOverlay;
IMPORT CktGraph; (* reveal CktElement.T *)
IMPORT CktElement;
IMPORT SpiceObject;
IMPORT DrawElements;
IMPORT Debug;
FROM SpiceAnalyze IMPORT DecodeTransistorTypeName;

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

    Debug.Out("PicComponent.Init: obj.src: " & SpiceObject.Format(obj.src));
    
    TYPECASE obj.src OF
      SpiceObject.R =>
      DrawElements.Res(t.under)
    |
      SpiceObject.C =>
      DrawElements.Cap(t.under)
    |
      SpiceObject.X(x) =>
      
      Debug.Error("Do not build component out of subcell of type " & x.type)
    |
      SpiceObject.M(m) =>
      WITH tt = DecodeTransistorTypeName(m.type) DO
        DrawElements.Fet(t.under, tt, FALSE)
      END
    ELSE
      Debug.Error("Unknown SpiceObject type")
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
