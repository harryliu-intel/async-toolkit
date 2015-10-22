INTERFACE SpiceCircuit;
IMPORT SpiceObjectSeq;
IMPORT Refany;
IMPORT TextSeq;

TYPE
  T = OBJECT
    name     : TEXT; (* if any *)
    params   : TextSeq.T;
    elements : SpiceObjectSeq.T;
  END;

CONST Brand = "SpiceCircuit";

CONST Equal = Refany.Equal;

END SpiceCircuit.
