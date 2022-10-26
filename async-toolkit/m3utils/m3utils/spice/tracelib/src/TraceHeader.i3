INTERFACE TraceHeader;

TYPE
  T = RECORD
    start  : CARDINAL;
    end    : CARDINAL;
    steps  : CARDINAL;
    nNodes : CARDINAL;
  END;

END TraceHeader.
