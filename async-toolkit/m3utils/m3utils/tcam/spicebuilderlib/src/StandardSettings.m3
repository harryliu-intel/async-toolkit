MODULE StandardSettings;
IMPORT ParseParams;
IMPORT SimDumper;
IMPORT SimParams, Random;
IMPORT Debug;
IMPORT Sim;
FROM Fmt IMPORT F, LongReal;

CONST LR = LongReal;

PROCEDURE New(pp : ParseParams.T; simParams : SimParams.T; rand : Random.T) : T 
  RAISES { ParseParams.Error }=

  TYPE
    AT = ARRAY OF TEXT;

  PROCEDURE PP(READONLY opts : AT; VAR x : LONGREAL)  RAISES { ParseParams.Error } =
    BEGIN
      FOR i := FIRST(opts) TO LAST(opts) DO
        IF pp.keywordPresent("-" & opts[i]) THEN x := pp.getNextLongReal() END
      END
    END PP;

  VAR
    s := NEW(T, simParams := simParams, rand := rand);
  BEGIN
    PP(AT { "clk" }                     , s.spd);
    PP(AT { "holdfrac" }                , s.holdFrac);
    PP(AT { "setupfrac" }               , s.setupFrac);
    PP(AT { "risefrac", "risefallfrac" }, s.riseFallFrac);
    
    PP(AT { "assertholdfrac" }          , s.assertHoldFrac);
    PP(AT { "assertholdtime" }          , s.assertHoldTime);

    s.cycleTime := 1.0d0 / s.spd;

    Debug.Out(F("cycleTime %s assertHoldFrac %s",
              LongReal(s.cycleTime),
              LongReal(s.assertHoldFrac)));

    IF pp.keywordPresent("-extractpath") THEN s.extractPath := pp.getNext() END;

    IF pp.keywordPresent("-spf") THEN s.spfParam := pp.getNextLongReal() END;

    s.verboseNodes := pp.keywordPresent("-verbosenodes");
    (**********************************************************************)

    IF s.spfParam # 0.0d0 THEN
      WITH minCap = 1.0d-16 / s.spfParam,
           minRes = 1.0d0 /        s.spfParam,
           ccapToGcap = 2.0d0 * minCap DO
        SimDumper.simExtras[Sim.T.XA].addhi(F(".OPTION XA_CMD=\"load_ba_file -file tcam.spf -min_cap %s -min_res %s -ccap_to_gcap %s\"",
                                              LR(minCap),
                                              LR(minRes),
                                              LR(ccapToGcap)))
      END
    END;

    s.cyc      := (1.0d0/s.spd);

    s.defConstraints.hold := s.holdFrac * s.cyc;
    s.defConstraints.setup := s.setupFrac * s.cyc;
    
    s.riseFall  := s.riseFallFrac * s.cyc;
    RETURN s
  END New;

BEGIN END StandardSettings.
