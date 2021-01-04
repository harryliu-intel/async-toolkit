MODULE SpiceDiagram;
IMPORT SpiceGateSeq;
IMPORT SpiceGate;
IMPORT Canvas;

REVEAL
  T = Public BRANDED Brand OBJECT
    gates : SpiceGateSeq.T;
  OVERRIDES
    init := Init;
    addGate := AddGate;
    render := Render;
  END;

PROCEDURE Init(t : T) : T =
  BEGIN
    t.gates := NEW(SpiceGateSeq.T).init();
    RETURN t
  END Init;

PROCEDURE AddGate(t : T; gate : SpiceGate.T) =
  BEGIN
    t.gates.addhi(gate)
  END AddGate;

PROCEDURE Render(t : T; to : Canvas.T) =
  BEGIN
  END Render;
  
BEGIN END SpiceDiagram.
