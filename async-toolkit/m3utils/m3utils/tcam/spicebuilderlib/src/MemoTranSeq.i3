INTERFACE MemoTranSeq;
IMPORT TranSeq;

TYPE T <: TranSeq.T;

PROCEDURE Interpolate(s : T; tm : LONGREAL) : LONGREAL;

END MemoTranSeq.
