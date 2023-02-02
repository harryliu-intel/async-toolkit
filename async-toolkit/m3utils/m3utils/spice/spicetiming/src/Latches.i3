INTERFACE Latches;
IMPORT Trace;

TYPE
  LatchNodes = RECORD
    clk, d, q : TEXT;
  END;

PROCEDURE IsQNode(trace          : Trace.T;
                  idx            : CARDINAL;
                  VAR latchNodes : LatchNodes) : BOOLEAN;

END Latches.
