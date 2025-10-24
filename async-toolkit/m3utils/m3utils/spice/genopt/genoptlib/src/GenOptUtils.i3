INTERFACE GenOptUtils;
IMPORT LRVector;
IMPORT Pathname;
IMPORT Wr;
IMPORT LongRealSeq AS LRSeq;
IMPORT LRVectorSeq;
FROM Fmt IMPORT Style;
FROM LongReal IMPORT MaxSignifDigits;

PROCEDURE FmtP(p : LRVector.T; style := Style.Auto; prec : CARDINAL := MaxSignifDigits - 1) : TEXT;

PROCEDURE MustOpenWr(pn : Pathname.T) : Wr.T;

PROCEDURE LRSeq1(x : LONGREAL) : LRSeq.T;

PROCEDURE LRVectorSeq1(x : LONGREAL; dims : CARDINAL) : LRVectorSeq.T;

PROCEDURE FmtLRSeq(seq : LRSeq.T; style := Style.Auto; prec : CARDINAL := MaxSignifDigits - 1) : TEXT;

PROCEDURE FmtLRVectorSeq(seq : LRVectorSeq.T; style := Style.Auto; prec : CARDINAL := MaxSignifDigits - 1) : TEXT;


END GenOptUtils.
