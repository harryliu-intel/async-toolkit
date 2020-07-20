INTERFACE ParseProcRec;
IMPORT ParseProc;

TYPE
  T = RECORD
    nm : TEXT;              (* name as TEXT *)
    f  : ParseProc.T;
    ca : REF ARRAY OF CHAR; (* name as CHAR array -- for END matching *)
  END;    

CONST Brand = "ParseProcRec";

CONST Default = T { "**TOP**", NIL, NIL };

END ParseProcRec.
