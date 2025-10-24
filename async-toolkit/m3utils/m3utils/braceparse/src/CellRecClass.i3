INTERFACE CellRecClass;
IMPORT CellRec;
IMPORT Subcell;
IMPORT MosInfoCardTbl;

TYPE
  Private = CellRec.Public OBJECT
    subcells : REF ARRAY OF Subcell.T;
    mosTbl   : MosInfoCardTbl.T;
    aux      : CARDINAL; (* value slot for client computations *)
  END;

REVEAL
  CellRec.T <: Private;

END CellRecClass.
