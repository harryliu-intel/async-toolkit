MODULE Src;
IMPORT Dims;
IMPORT DimsTranSeqTbl;
IMPORT MemoTranSeq;
IMPORT TranSeq;

REVEAL
  T = Public BRANDED OBJECT
  OVERRIDES
    getV := SIGetV;
    getSeq := GetSeq;
    infinite := DefInfinite;
  END;

PROCEDURE DefInfinite(<*UNUSED*>t : T) : BOOLEAN = 
  BEGIN RETURN FALSE END DefInfinite;

PROCEDURE SIGetV(self : T; READONLY d : Dims.T; t : LONGREAL) : LONGREAL =
  BEGIN
    WITH seq = self.getSeq(d) DO
      RETURN MemoTranSeq.Interpolate(seq, t)
    END
  END SIGetV;

PROCEDURE GetSeq(self : T; READONLY d : Dims.T) : MemoTranSeq.T =
  (* memoizer *)
  VAR
    seq : TranSeq.T;
  BEGIN
    IF self.trans = NIL THEN
      self.trans := NEW(DimsTranSeqTbl.Default).init()
    END;
    IF NOT self.trans.get(d, seq) THEN
      seq := self.makeSeq(d);
      EVAL self.trans.put(d, seq)
    END;
    RETURN seq
  END GetSeq;

BEGIN END Src.
