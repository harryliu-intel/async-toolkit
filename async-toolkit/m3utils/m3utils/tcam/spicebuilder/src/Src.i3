INTERFACE Src;
IMPORT Dims, DimsTranSeqTbl;
IMPORT Intf;
IMPORT MemoTranSeq;

TYPE
  T <: Public;

  Public = Intf.T OBJECT    
    trans : DimsTranSeqTbl.T := NIL;
  METHODS
    getV(READONLY dims : Dims.T; t : LONGREAL) : LONGREAL;
    getSeq(READONLY dims : Dims.T) : MemoTranSeq.T;

    (* subtypes, implement the following! *)
    makeSeq(READONLY dims : Dims.T) : MemoTranSeq.T;

    infinite() : BOOLEAN; (* default false *)
  END;

CONST Brand = "Src";

END Src.
