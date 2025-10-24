INTERFACE SpiceGate;
IMPORT FetArray;

TYPE
  Pull = { Down, Up };

  T = ARRAY Pull OF FetArray.T;

CONST Brand = "SpiceGate";

END SpiceGate.
