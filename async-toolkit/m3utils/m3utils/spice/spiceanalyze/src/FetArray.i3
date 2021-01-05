INTERFACE FetArray;
IMPORT CktElement;
IMPORT CktElementSeq;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init() : T;

    addToRow(e : CktElement.T; row : CARDINAL);
    (* 0 is the row nearest the output (drain end of network) --
       increases towards the power supply *)

    size() : CARDINAL;
    getRow(row : CARDINAL) : Row;
  END;

  Row = CktElementSeq.T;

CONST Brand = "FetArray";

END FetArray.
