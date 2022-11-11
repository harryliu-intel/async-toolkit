INTERFACE TraceRep;
IMPORT Trace;
IMPORT TextCardTbl;
IMPORT CardTextSeqTbl;

REVEAL
  Trace.T <: Private;

TYPE
  Private = Trace.Public OBJECT
    fwdTbl   : TextCardTbl.T;
    revTbl   : CardTextSeqTbl.T;
  END;

END TraceRep.
