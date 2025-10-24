INTERFACE VectorSeq;

IMPORT LRVectorSeq;
IMPORT Pathname;
IMPORT MultiEvalLRVector;

TYPE T = LRVectorSeq.T;

PROCEDURE ToMulti(seq        : T;
                  subdirPath : Pathname.T) : MultiEvalLRVector.Result;

END VectorSeq.
