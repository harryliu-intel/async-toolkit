INTERFACE SimParams;

TYPE
  T = RECORD
    step    : LONGREAL;
    maxTime : LONGREAL; (* overridden by length of sequence *)
  END;

END SimParams.
