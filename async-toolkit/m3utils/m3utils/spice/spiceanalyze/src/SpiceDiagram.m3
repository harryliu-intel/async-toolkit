MODULE SpiceDiagram;
IMPORT SpiceGateSeq;
IMPORT SpiceGate;
IMPORT Canvas;

REVEAL
  T = Public BRANDED Brand OBJECT
    gates : SpiceGateSeq.T;
  OVERRIDES
    init    := Init;
    addGate := AddGate;
    render  := Render;
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
  CONST
    InterGateGap = 20.0d0;
  VAR
    offset := 0.0d0;
  BEGIN
    (* we will just render the gates left to right to begin with *)
    FOR i := 0 TO t.gates.size() - 1 DO
      offset := RenderGate(to, t.gates.get(i), offset) + InterGateGap
    END
  END Render;

PROCEDURE RenderGate(to     : Canvas.T;
                     g      : SpiceGate.T;
                     offset : LONGREAL) : LONGREAL =
  (* returns urx of just-rendered gate *)
  BEGIN
    RETURN offset
  END RenderGate;
  
BEGIN END SpiceDiagram.
