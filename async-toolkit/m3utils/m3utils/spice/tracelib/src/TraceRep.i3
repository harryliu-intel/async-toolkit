INTERFACE TraceRep;
IMPORT Trace;
IMPORT TextCardTbl;
IMPORT CardTextSeqTbl;

(* 
   Reveal the internal alias dictionaries of the Trace.T representation 

   Not sure this actually needs to be revealed to any clients---the 
   interface provided by Trace.T ought to be sufficient for any reasonable
   purposes.
*)

REVEAL
  Trace.T <: Private;

TYPE
  Private = Trace.Public OBJECT
    fwdTbl   : TextCardTbl.T;
    revTbl   : CardTextSeqTbl.T;
  END;

END TraceRep.
