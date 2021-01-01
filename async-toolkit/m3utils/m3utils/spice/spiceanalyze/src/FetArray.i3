INTERFACE FetArray;
IMPORT CktElement;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init() : T;
    addToRow(e : CktElement.T; row : CARDINAL);
  END;

CONST Brand = "FetArray";

END FetArray.
