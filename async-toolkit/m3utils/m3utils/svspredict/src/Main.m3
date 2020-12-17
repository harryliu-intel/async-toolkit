MODULE Main;
IMPORT Random, NormalDeviate;
IMPORT Debug;
IMPORT LongrealArraySort;
FROM Fmt IMPORT Int, F;
IMPORT Fmt;
IMPORT Math;
IMPORT FileWr, Wr;
IMPORT Thread, OSError;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT Pathname;
FROM SvsTypes IMPORT CornerData, ProgramSetter;
IMPORT Text;
IMPORT Cloudbreak;
IMPORT JBay;

<*FATAL Thread.Alerted, Wr.Failure, OSError.E*>

CONST LR = Fmt.LongReal;
      TE = Text.Equal;

VAR Ss, Tt, Ff              : CornerData;
VAR RefP, FixedP, RefLeakP  : LONGREAL;
VAR LkgRatio, LkgRatioSigma : LONGREAL;
VAR Trunc                   : LONGREAL;

VAR
  H       :=   15; (* # of buckets *)
  Samples := 1000; (* # of samples *)

PROCEDURE Interpolate1(x : LONGREAL;
                       s, ssigma, t, tsigma, f, fsigma : LONGREAL) : LONGREAL=
  BEGIN
    (* 
       negative -> FAST 
       positive -> SLOW
    *)
    IF       x <  tsigma THEN
      RETURN x * (f - t) / (fsigma - tsigma) + t
    ELSE  (* x >= tsigma *)
      RETURN x * (s - t) / (ssigma - tsigma) + t
    END
  END Interpolate1;
  
PROCEDURE Interpolate(x : LONGREAL) : CornerData =
  VAR
    res : CornerData;
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
  
PROCEDURE MakeDie() : CornerData =
  (* returns the PVT voltage of a single die *)
  BEGIN
    LOOP
      VAR
        x := NormalDeviate.Get(rand, 0.0d0, 1.0d0);
      BEGIN
        IF ABS(x) <= Trunc THEN
          RETURN Interpolate(x)
        END
      END
    END
  END MakeDie;

VAR rand := NEW(Random.Default).init();

PROCEDURE CalcPower(at : CornerData) : LONGREAL =
  BEGIN
    (* +sigma = slow, less leaky *)
    WITH RefRestPwr    = RefP - FixedP - RefLeakP,
         cornerLkgRatio = Math.pow(LkgRatio, -at.sigma / LkgRatioSigma),
         voltPwrRatio = (at.vpower/Tt.vpower)*(at.vpower/Tt.vpower),
         restPwr = RefRestPwr * voltPwrRatio,
         leakPwr = RefLeakP * cornerLkgRatio * voltPwrRatio,
         totPwr  = FixedP + restPwr + leakPwr DO
      IF oWr # NIL THEN
        Wr.PutText(oWr, F("%s %s %s %s %s\n",
                          LR(at.sigma),
                          LR(at.vpower),
                          LR(cornerLkgRatio),
                          LR(leakPwr),
                          LR(totPwr)))
      END;
      RETURN totPwr
    END
  END CalcPower;
    
PROCEDURE DoIt() = 
  VAR
    res  := NEW(REF ARRAY OF LONGREAL, Samples);
  BEGIN
    FOR i := FIRST(res^) TO LAST(res^) DO
      WITH corner = MakeDie() DO
        res[i] := CalcPower(corner)
      END
    END;
    
    (* make histogram of res *)
    LongrealArraySort.Sort(res^);
    DoHistogram(res^);
  END DoIt;

PROCEDURE DoHistogram(READONLY res : ARRAY OF LONGREAL (* sorted *)) =
  VAR
    min := res[FIRST(res)];
    max := res[LAST (res)];
    hw  := (max - min) / FLOAT(H, LONGREAL);
    hcnt := NEW(REF ARRAY OF CARDINAL, H);
    hmax := 0.0d0;
    sum, sumsq := 0.0d0;
    n := FLOAT(NUMBER(res),LONGREAL);
  BEGIN
    FOR i := FIRST(hcnt^) TO LAST(hcnt^) DO
      hcnt[i] := 0
    END;
    FOR i := FIRST(res) TO LAST(res) DO
      sum := sum + res[i];
      sumsq := sumsq + res[i] * res[i];
      WITH h  = (res[i] - min) / hw,
           ht = TRUNC(h) DO
        hmax := MAX(h,hmax);
        INC(hcnt[MIN(ht,LAST(hcnt^))])
        (* the MAX is FOR a round-off possibility pushing us up... *)
      END
    END;
    
    Debug.Out(F("hmax = %s, H = %s", LR(hmax), Int(H)));
    
    (* dump the histogram *)
    WITH wr  = FileWr.Open(ofn & "_hist.dat") DO
      FOR i := FIRST(hcnt^) TO LAST(hcnt^) DO
        WITH lo = min + hw * FLOAT(i    , LONGREAL),
             hi = min + hw * FLOAT(i + 1, LONGREAL),
             c  = hcnt[i] DO
          Debug.Out(F("hist bin %s from %s to %s cnt %s",
                      Int(i),
                      LR(lo),
                      LR(hi),
                      Int(c)));
          
          Wr.PutText(wr, F("%s %s\n", LR(lo), "0.0"));
          Wr.PutText(wr, F("%s %s\n", LR(hi), "0.0"));
          Wr.PutText(wr, F("%s %s\n", LR(hi), Int(c)));
          Wr.PutText(wr, F("%s %s\n", LR(lo), Int(c)));
          Wr.PutText(wr, F("%s %s\n", LR(lo), "0.0"));
          Wr.PutText(wr, "\n");
        END
      END;
      Wr.Close(wr)
    END;

    
    WITH lwr = FileWr.Open(ofn & "_loss.dat") DO
      FOR i := FIRST(res) TO LAST(res) DO
        WITH rem = 1.0d0 - FLOAT(i+1,LONGREAL)/ n DO
          Wr.PutText(lwr, F("%s %s\n", LR(res[i]), LR(rem)))
        END
      END;
      Wr.Close(lwr)
    END;
    
    WITH mean   = sum   / n,
         meansq = sumsq / n,
         var    = n / (n - 1.0d0) * (meansq - mean * mean),
         sdev   = Math.sqrt(var) DO
      Debug.Out(F("mean %s sdev %s", LR(mean), LR(sdev)))
    END
  END DoHistogram;
  

VAR
  pp := NEW(ParseParams.T).init(Stdio.stderr);
  ofn : Pathname.T := "hist";
  oWr : Wr.T := NIL;
BEGIN
  
  TRY
    IF pp.keywordPresent("-d") OR pp.keywordPresent("-program") THEN
      VAR
        f : ProgramSetter;
        progName := pp.getNext();
      BEGIN
        IF    TE(progName, "Cloudbreak") THEN
          f := Cloudbreak.SetProgram
        ELSIF TE(progName, "JBay") THEN
          f := JBay.SetProgram
        ELSE
          Debug.Error(F("Unknown BXD program \"%s\"", progName))
        END;
        f(Ss, Tt, Ff, RefP, FixedP, RefLeakP, LkgRatio, LkgRatioSigma, Trunc)
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

    IF pp.keywordPresent("-Vsspower") THEN
      Ss.vpower := pp.getNextLongReal()
    END;
    
    IF pp.keywordPresent("-Vffpower") THEN
      Ff.vpower := pp.getNextLongReal()
    END;

    IF pp.keywordPresent("-Vttpower") THEN
      Tt.vpower := pp.getNextLongReal()
    END;

    IF pp.keywordPresent("-lkgratio") THEN
      LkgRatio := pp.getNextLongReal()
    END;

    IF pp.keywordPresent("-o") THEN
      ofn := pp.getNext()
    END;
    
    pp.skipParsed()

  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command line")
  END;
  
  DoIt();

  IF oWr # NIL THEN Wr.Close(oWr) END
 
END Main.
