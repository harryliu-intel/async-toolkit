INTERFACE TraceUnsafe;
IMPORT TraceHeader;
IMPORT Rd;

PROCEDURE GetHeader(tRd : Rd.T; nNodes : CARDINAL) : TraceHeader.T
  RAISES { Rd.Failure, Rd.EndOfFile };

PROCEDURE GetDataArray(tRd        : Rd.T;
                       READONLY h : TraceHeader.T;
                       id         : CARDINAL;
                       VAR arr    : ARRAY OF LONGREAL)
  RAISES { Rd.Failure, Rd.EndOfFile };

PROCEDURE GetBytes(rd : Rd.T; bytes : CARDINAL) : TEXT
  RAISES { Rd.Failure, Rd.EndOfFile };
  
END TraceUnsafe.
