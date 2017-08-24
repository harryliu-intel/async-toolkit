INTERFACE StandardSettings;
IMPORT ParseParams;
IMPORT SimParams;
IMPORT Random;

TYPE
  Constraints = RECORD
    setup : LONGREAL;
    hold  : LONGREAL;
  END;

  T = OBJECT (* make it so NIL object will crash programs *)
    extractPath : TEXT;
    spd := FIRST(LONGREAL);
    riseFallFrac := 0.1d0;
    setupFrac := 0.2d0;
    holdFrac := 0.3d0;
    cyc, spfParam : LONGREAL;
    verboseNodes : BOOLEAN;
    assertHoldFrac := 0.0d0; (* ugly global! -- not a permanent solution *)
    assertHoldTime := 0.0d0; (* ugly global! -- not a permanent solution *)
    cycleTime := LAST(LONGREAL); (* ditto *)
    riseFall  := LAST(LONGREAL);
    defConstraints : Constraints;
    simParams : SimParams.T;
    rand : Random.T;
  END;

PROCEDURE New(pp : ParseParams.T; simParams : SimParams.T; rand : Random.T) : T
  RAISES { ParseParams.Error };

END StandardSettings.
