INTERFACE SpiceDiagram;
IMPORT Canvas, SpiceGate;
TYPE
  T <: Public;

  Public = OBJECT METHODS
    init() : T;
    addGate(gate : SpiceGate.T);
    render(to : Canvas.T);
  END;

CONST Brand = "SpiceDiagram";

END SpiceDiagram.
