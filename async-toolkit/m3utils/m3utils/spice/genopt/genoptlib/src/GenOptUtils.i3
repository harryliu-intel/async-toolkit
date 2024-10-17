INTERFACE GenOptUtils;
IMPORT LRVector;
IMPORT Pathname;
IMPORT Wr;
IMPORT LongRealSeq AS LRSeq;

PROCEDURE FmtP(p : LRVector.T) : TEXT;

PROCEDURE MustOpenWr(pn : Pathname.T) : Wr.T;

PROCEDURE LRSeq1(x : LONGREAL) : LRSeq.T;

PROCEDURE FmtLRSeq(seq : LRSeq.T) : TEXT;


END GenOptUtils.
