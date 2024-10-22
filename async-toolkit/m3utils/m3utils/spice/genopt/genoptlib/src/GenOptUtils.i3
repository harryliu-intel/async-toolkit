INTERFACE GenOptUtils;
IMPORT LRVector;
IMPORT Pathname;
IMPORT Wr;
IMPORT LongRealSeq AS LRSeq;
IMPORT LRVectorSeq;

PROCEDURE FmtP(p : LRVector.T) : TEXT;

PROCEDURE MustOpenWr(pn : Pathname.T) : Wr.T;

PROCEDURE LRSeq1(x : LONGREAL) : LRSeq.T;

PROCEDURE LRVectorSeq1(x : LONGREAL; dims : CARDINAL) : LRVectorSeq.T;

PROCEDURE FmtLRSeq(seq : LRSeq.T) : TEXT;

PROCEDURE FmtLRVectorSeq(seq : LRVectorSeq.T) : TEXT;


END GenOptUtils.
