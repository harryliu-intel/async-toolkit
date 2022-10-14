INTERFACE TraceHeader;

TYPE
  T = RECORD
    start  : CARDINAL;
    end    : CARDINAL;
    steps  : CARDINAL;
    nNames : CARDINAL;
  END;

END TraceHeader.
