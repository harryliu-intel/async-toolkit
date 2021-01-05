MODULE Main;
IMPORT Random, NormalDeviate;
IMPORT Debug;
IMPORT LongrealArraySort;
FROM Fmt IMPORT F, FN;
IMPORT Fmt;
IMPORT FileWr, Wr;
IMPORT Thread, OSError;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT Pathname;
FROM SvsTypes IMPORT CornerData;
IMPORT Text;
IMPORT Cloudbreak;
IMPORT JBay;
IMPORT Power;
IMPORT Corner;
IMPORT ProgramSetter;
IMPORT Histogram;
IMPORT LRFunction;
IMPORT Solve;

<*FATAL Thread.Alerted, Wr.Failure, OSError.E*>

CONST LR = Fmt.LongReal;
      TE = Text.Equal;

VAR Trunc : LONGREAL;

VAR Samples := 1000; (* # of samples *)

PROCEDURE Interpolate1(x : LONGREAL;
                       s, ssigma, t, tsigma, f, fsigma : LONGREAL) : LONGREAL=
  BEGIN
    (* 
       negative -> SLOW 
       positive -> FAST
    *)
    IF       x >=  tsigma THEN
      RETURN x * (f - t) / (fsigma - tsigma) + t
    ELSE  (* x < tsigma *)
      RETURN x * (s - t) / (ssigma - tsigma) + t
    END
  END Interpolate1;
  
PROCEDURE Interpolate(READONLY p : Power.Params;
                               x : LONGREAL) : CornerData =
  VAR
    res : CornerData;
    Ss := p.c[Corner.T.SS];
    Tt := p.c[Corner.T.TT];
    Ff := p.c[Corner.T.FF];
  BEGIN
    res.sigma := x;
    res.vtiming := Interpolate1(x,
                                Ss.vtiming, Ss.sigma,
                                Tt.vtiming, Tt.sigma,
                                Ff.vtiming, Ff.sigma);
    res.vpower  := Interpolate1(x,
                                Ss.vpower, Ss.sigma,
                                Tt.vpower, Tt.sigma,
                                Ff.vpower, Ff.sigma);
    RETURN res
  END Interpolate;
  
PROCEDURE MakeDie(READONLY p : Power.Params) : CornerData =
  (* returns the PVT voltage of a single die *)
  BEGIN
    LOOP
      VAR
        x := NormalDeviate.Get(rand, 0.0d0, 1.0d0);
      BEGIN
        IF ABS(x) <= Trunc THEN
          RETURN Interpolate(p, x)
        END
      END
    END
  END MakeDie;

VAR rand := NEW(Random.Default).init();

PROCEDURE DoDebugCorner(READONLY p : Power.Params) =
  VAR
    dbgCorner := Interpolate(p, 0.0d0);
    q : Power.Result;
  BEGIN
    dbgCorner.vpower := 0.750d0;
    
    q := Power.Calc(p, dbgCorner);
    Debug.Out(F("DEBUG CORNER Vdd=%s leakPwr=%s totPwr=%s",
                LR(dbgCorner.vpower), LR(q.leakPwr), LR(q.totPwr)))
    
  END DoDebugCorner;
    
PROCEDURE DoIt(READONLY p : Power.Params) = 
  VAR
    totPwr  := NEW(REF ARRAY OF LONGREAL, Samples);
    deltaV  := NEW(REF ARRAY OF LONGREAL, Samples);
    dist := p;
    dvp  : LONGREAL := 0.0d0;
    shP : Power.Result;
  BEGIN
    DoDebugCorner(p);
    
    FOR i := FIRST(totPwr^) TO LAST(totPwr^) DO
      WITH corner = MakeDie(dist),
           p      = Power.Calc(dist, corner) DO

        shP := p;
        
        IF solveMax # FIRST(LONGREAL) THEN
          WITH f = NEW(SolveDeltaV,
                       constraintP := solveMax,
                       dist        := dist,
                       corner      := corner) DO
            dvp := Solve.WDB(f, -0.1d0, +0.1d0);
            deltaV[i] := dvp;
            VAR
              shiftCorner := corner;
            BEGIN
              shiftCorner.vpower := corner.vpower + dvp;
              shP := Power.Calc(dist, shiftCorner);
            END            
          END
        END;
        
        IF oWr # NIL THEN
          Wr.PutText(oWr, FN("%s %s %s %s %s %s %s %s\n",ARRAY OF TEXT{
                            LR(corner.sigma),
                            LR(corner.vpower),
                            LR(p.cornerLkgRatio),
                            LR(p.leakPwr),
                            LR(p.totPwr),

                            LR(dvp),

                            LR(shP.leakPwr),
                            LR(shP.totPwr)    }))
        END;
        totPwr[i] := p.totPwr
      END
    END;
    
    (* make power histogram *)
    LongrealArraySort.Sort(totPwr^);
    Histogram.Do(ofn, totPwr^, TRUE, H := H);

    IF solveMax # FIRST(LONGREAL) THEN
      (* make deltaV historgram *)
      LongrealArraySort.Sort(deltaV^);
      Histogram.Do(ofn & "_deltav", deltaV^, FALSE, H := H);
    END
  END DoIt;

TYPE
  SolveDeltaV = LRFunction.T OBJECT
    constraintP : LONGREAL;
    corner      : CornerData;
    dist        : Power.Params;
  OVERRIDES
    eval := SolveDeltaVEval;
  END;

PROCEDURE SolveDeltaVEval(state : SolveDeltaV; dv : LONGREAL) : LONGREAL =
  VAR
    corner := state.corner;
  BEGIN
    corner.vpower := state.corner.vpower + dv;
    RETURN Power.Calc(state.dist, corner).totPwr - state.constraintP
  END SolveDeltaVEval;

VAR
  pp := NEW(ParseParams.T).init(Stdio.stderr);
  ofn : Pathname.T := "hist";
  oWr : Wr.T := NIL;
  H := Histogram.DefaultBuckets;
  p   : Power.Params;
  solveMax := FIRST(LONGREAL);
  
BEGIN
  
  TRY
    IF pp.keywordPresent("-d") OR pp.keywordPresent("-program") THEN
      VAR
        f : ProgramSetter.T;
        progName := pp.getNext();
      BEGIN
        IF    TE(progName, "Cloudbreak") THEN
          f := Cloudbreak.SetProgram
        ELSIF TE(progName, "JBay") THEN
          f := JBay.SetProgram
        ELSE
          Debug.Error(F("Unknown BXD program \"%s\"", progName))
        END;
        f(p, Trunc);

        Debug.Out(F("Set program %s :", progName));
        Debug.Out(F("p="));
        Debug.Out(Power.FmtParams(p));
        Debug.Out(F("Trunc= %s", LR(Trunc)))
      END
    END;
    
    IF pp.keywordPresent("-a") THEN
      oWr := FileWr.Open(pp.getNext())
    END;

    IF pp.keywordPresent("-H") THEN
      H := pp.getNextInt()
    END;

    IF pp.keywordPresent("-trunc") THEN
      Trunc := pp.getNextLongReal()
    END;

    IF pp.keywordPresent("-samples") THEN
      Samples := pp.getNextInt()
    END;

    IF pp.keywordPresent("-solvemax") THEN
      solveMax := pp.getNextLongReal()
    END;

    IF pp.keywordPresent("-Vsspower") THEN
      p.c[Corner.T.SS].vpower := pp.getNextLongReal()
    END;
    
    IF pp.keywordPresent("-Vffpower") THEN
      p.c[Corner.T.FF].vpower := pp.getNextLongReal()
    END;

    IF pp.keywordPresent("-Vttpower") THEN
      p.c[Corner.T.TT].vpower := pp.getNextLongReal()
    END;

    IF pp.keywordPresent("-lkgratio") THEN
      p.LkgRatio := pp.getNextLongReal()
    END;

    IF pp.keywordPresent("-o") THEN
      ofn := pp.getNext()
    END;
    
    pp.skipParsed()

  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command line")
  END;
  
  DoIt(p);

  IF oWr # NIL THEN Wr.Close(oWr) END
 
END Main.
