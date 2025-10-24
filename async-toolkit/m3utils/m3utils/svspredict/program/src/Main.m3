(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE Main;
IMPORT Random;
IMPORT Debug;
IMPORT LongrealArraySort;
IMPORT LongRealSeq AS LRSeq;
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
IMPORT Die;
IMPORT DieArraySort;
FROM PowerScaling IMPORT Interpolate;
IMPORT IO;

<*FATAL Thread.Alerted, Wr.Failure, OSError.E*>

TYPE
  Array = ARRAY OF LONGREAL;
  
CONST LR = Fmt.LongReal;
      TE = Text.Equal;

VAR Trunc : LONGREAL;

VAR Samples := 1000; (* # of samples *)

VAR rand := NEW(Random.Default).init();

PROCEDURE DoIt(READONLY p : Power.Params) : ShipDist = 
  VAR
    dice    := NEW(REF ARRAY OF Die.T, Samples);
    totPwr  := NEW(REF Array, Samples);
    deltaV  := NEW(REF Array, Samples);
    dist := p;
    dvp  : LONGREAL := 0.0d0;
    shP : Power.Result;
    res :=  ShipDist { worstSigma := -Trunc,
                       medianSigma := 0.0d0 };
  BEGIN
    Power.DoDebugCorner(p);
    
    FOR i := FIRST(totPwr^) TO LAST(totPwr^) DO
      WITH corner = Power.MakeDie(rand, dist, Trunc),
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

        dice[i] := Die.T { corner.sigma, p.totPwr, dvp };
        
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
    DieArraySort.Sort(dice^, Die.ComparePower);
    
    LongrealArraySort.Sort(totPwr^);
    Histogram.Do(ofn,
                 Die.ExtractValues(dice^,
                                   Die.Value.Power)^, TRUE, H := H);

    IF solveMax # FIRST(LONGREAL) THEN
      (* make deltaV histogram *)
      LongrealArraySort.Sort(deltaV^);
      Histogram.Do(ofn & "_deltav", deltaV^, FALSE, H := H);
    END;

    IF minSigma # FIRST(LONGREAL) THEN
      (* also make the cut off distribution *)
      VAR
        cutoff := Die.CutoffArr(dice^, Die.Value.Sigma, minSigma, TRUE);
        
        cutoffPwr   := Die.ExtractValues(cutoff^, Die.Value.Power);
        cutoffSig   := Die.ExtractValues(cutoff^, Die.Value.Sigma);
        
        medianPwr := Median(cutoffPwr^);
        medianSig := Median(cutoffSig^);
        yield     := FLOAT(NUMBER(cutoffPwr^), LONGREAL) /
                     FLOAT(NUMBER(totPwr^), LONGREAL);
      BEGIN
        Histogram.Do(ofn & "_cutoff", cutoffPwr^, FALSE, H := H);
        Debug.Out("Yield = " & LR(yield));
        Debug.Out("Median power of cut-off dist " & LR(medianPwr));
        Debug.Out("Median sigma of cut-off dist " & LR(medianSig));
        res.medianSigma := medianSig;
        res.worstSigma  := cutoff[LAST(cutoff^)][Die.Value.Sigma];
      END
    END;
    RETURN res
  END DoIt;

TYPE
  ShipDist = RECORD
    worstSigma, medianSigma : LONGREAL;
  END;

PROCEDURE CutoffArr(READONLY a : Array;
                    max        : LONGREAL ) : REF Array =
  VAR
    seq := NEW(LRSeq.T).init();
    res : REF Array;
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      WITH p = a[i] DO
        IF p <= max THEN
          seq.addhi(p)
        END
      END
    END;
    res := NEW(REF Array, seq.size());
    FOR i := 0 TO seq.size() - 1 DO
      res[i] := seq.get(i)
    END;
    RETURN res
  END CutoffArr;

PROCEDURE Median(READONLY a : Array) : LONGREAL =
  VAR
    mid := LAST(a) DIV 2;
  BEGIN
    CASE NUMBER(a) MOD 2 OF
      0 => RETURN (a[mid] + a[mid+1]) / 2.0d0
    |
      1 => RETURN a[mid]
    END
  END Median;

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

TYPE
  SolvePower = LRFunction.T OBJECT
    p           : Power.Params;
    constraintP : LONGREAL;
  OVERRIDES
    eval := SolvePowerEval;
  END;

PROCEDURE SolvePowerEval(state : SolvePower; sigma : LONGREAL) : LONGREAL =
  BEGIN
    RETURN JBay.EvalCustomerMaxSpec(state.p, sigma) - state.constraintP
  END SolvePowerEval;
  
VAR
  pp                  := NEW(ParseParams.T).init(Stdio.stderr);
  ofn : Pathname.T    := "hist";
  oWr : Wr.T          := NIL;
  H                   := Histogram.DefaultBuckets;
  p   : Power.Params;
  solveMax, custMaxPower, minSigma := FIRST(LONGREAL);

  progName : TEXT;
  
BEGIN
  
  TRY
    IF pp.keywordPresent("-d") OR pp.keywordPresent("-program") THEN
      progName := pp.getNext();
      VAR
        f : ProgramSetter.T;
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

    IF pp.keywordPresent("-custmaxpower") THEN
      custMaxPower := pp.getNextLongReal()
    END;

    IF pp.keywordPresent("-solvemax") THEN
      IF custMaxPower # FIRST(LONGREAL) THEN
        Debug.Error("Don't specify both -custmaxpower and -solvemax")
      END;
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

  IF TE(progName, "JBay") AND custMaxPower # FIRST(LONGREAL) THEN
    VAR
      pm3 := JBay.EvalCustomerMaxSpec(p, -3.0d0);
      p0  := JBay.EvalCustomerMaxSpec(p,  0.0d0);
      pp3 := JBay.EvalCustomerMaxSpec(p, +3.0d0);
    BEGIN
      Debug.Out(F("pm3 %s p0 %s pp3 %s", LR(pm3), LR(p0), LR(pp3)));

      WITH f = NEW(SolvePower,
                   constraintP := custMaxPower,
                   p           := p) DO
        minSigma := Solve.WDB(f, -3.0d0, +3.0d0);
        Debug.Out("MinSigma = " & LR(minSigma));

        WITH pwr = Power.Calc(p, Interpolate(p, minSigma)) DO
          IO.Put("DIECUTOFF " & LR(pwr.totPwr) & "\n");
          solveMax := pwr.totPwr;
        END
      END
    END
  END;

  WITH shipData = DoIt(p) DO
  
    IF oWr # NIL THEN Wr.Close(oWr) END;
    
    IF TE(progName, "JBay") THEN
      Debug.Out("********************   JBAY SPECIAL CASES   ********************");
      WITH wr = FileWr.Open("jbay_special.csv") DO
        JBay.EvalSpecialCases(wr,
                              p,
                              shipData.medianSigma,
                              shipData.worstSigma);
        Wr.Close(wr)
      END
    END
  END

END Main.
