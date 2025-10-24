(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE Main;
IMPORT Random, NormalDeviate;
IMPORT Debug;
IMPORT LongRealSeq;
IMPORT LongrealArraySort;
FROM Fmt IMPORT Int, F;
IMPORT Fmt;
IMPORT Math;
IMPORT FileWr, Wr;
IMPORT Thread, OSError;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT Pathname;

<*FATAL Thread.Alerted, Wr.Failure, OSError.E*>

CONST LR = Fmt.LongReal;

TYPE
  Corner = RECORD
    v, sigma : LONGREAL;
  END;
  
VAR
  Vssgnp := Corner { 0.825d0, +3.0d0 };
  Vtt    := Corner { 0.750d0,  0.0d0 };
  Vffgnp := Corner { 0.675d0, -3.0d0 };

VAR
  K :=  1; (* number of bins *)
  N :=  2; (* number of dice per MCP *)
  H := 15;

  Trunc := 3.0d0; (* where to truncate the distribution at sort *)
  
  SamplesPerBin := 20000;

PROCEDURE MakeDie() : LONGREAL =
  (* returns the PVT voltage of a single die *)
  BEGIN
    LOOP
      VAR
        x := NormalDeviate.Get(rand, 0.0d0, 1.0d0);
      BEGIN
        IF ABS(x) <= Trunc THEN
          VAR
            v : LONGREAL;
          BEGIN
            IF       x <  0.0d0 THEN
              v := x * (Vffgnp.v - Vtt.v) / (Vffgnp.sigma - Vtt.sigma) + Vtt.v
            ELSE  (* x >= 0.0d0 *)
              v := x * (Vssgnp.v - Vtt.v) / (Vssgnp.sigma - Vtt.sigma) + Vtt.v
            END;
            RETURN v
          END
        END
      END
    END
  END MakeDie;

VAR rand := NEW(Random.Default).init();

PROCEDURE RemoveOne(bin : LongRealSeq.T) : LONGREAL =
  VAR
    whch := rand.integer(0, bin.size() - 1);
    res := bin.get(whch);
  BEGIN
    bin.put(whch, bin.get(0));
    EVAL bin.remlo();
    RETURN res
  END RemoveOne;
    
PROCEDURE DoSample(bin : LongRealSeq.T) : LONGREAL =
  VAR
    dice, power := NEW(REF ARRAY OF LONGREAL, N);
    sumpower, max := 0.0d0;
    
  BEGIN
    FOR i := FIRST(dice^) TO LAST(dice^) DO
      dice[i] := RemoveOne(bin);
      (*Debug.Out(F("dice[%s] = %s", Int(i), LR(dice[i])));*)
      max := MAX(max, dice[i])
    END;

    (* compute power of each die *)
    (* assume power is 1 at the ideal setpoint of die.  Power is quadratic
       with voltage *)

    FOR i := FIRST(dice^) TO LAST(dice^) DO
      <*ASSERT max >= dice[i]*>
      power[i] := max*max/dice[i]/dice[i];
      sumpower := sumpower + power[i]
    END;
    (*Debug.Out(F("sumpower %s", LR(sumpower)));*)
    RETURN sumpower
  END DoSample;

PROCEDURE DoIt() = 
  VAR
    res  := NEW(REF ARRAY OF LONGREAL, SamplesPerBin * K);
    fab  := NEW(REF ARRAY OF LONGREAL, SamplesPerBin * K * N);
    bins := NEW(REF ARRAY OF LongRealSeq.T, K);
  BEGIN
    FOR i := FIRST(fab^) TO LAST(fab^) DO
      fab[i] := MakeDie()
    END;

    LongrealArraySort.Sort(fab^);
    
    VAR
      j := 0;
    BEGIN
      FOR b := 0 TO K - 1 DO
        bins[b] := NEW(LongRealSeq.T).init();
        FOR i := 0 TO SamplesPerBin * N - 1 DO
          bins[b].addhi(fab[j]);
          INC(j)
        END;
        Debug.Out(F("bin %s : min %s max %s n %s",
                    Int(b),
                    LR(bins[b].get(0)),
                    LR(bins[b].get(bins[b].size()-1)),
                    Int(bins[b].size())));
      END;
      <*ASSERT j = NUMBER(fab^)*>
    END;
    
    VAR
      j := 0;
    BEGIN
      FOR b := 0 TO K - 1 DO
        FOR i := 0 TO SamplesPerBin - 1 DO
          res[j] := DoSample(bins[b]) / FLOAT(N, LONGREAL);
          INC(j)
        END
      END
    END;
    
    (* make histogram of res *)
    
    LongrealArraySort.Sort(res^);
    
    VAR
      min := res[FIRST(res^)];
      max := res[LAST (res^)];
      hw  := (max - min) / FLOAT(H, LONGREAL);
      hcnt := NEW(REF ARRAY OF CARDINAL, H);
      hmax := 0.0d0;
      sum, sumsq := 0.0d0;
    BEGIN
      FOR i := FIRST(hcnt^) TO LAST(hcnt^) DO
        hcnt[i] := 0
      END;
      FOR i := FIRST(res^) TO LAST(res^) DO
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
      WITH wr = FileWr.Open(ofn) DO
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
      
      WITH n      = FLOAT(NUMBER(res^),LONGREAL),
           mean   = sum   / n,
           meansq = sumsq / n,
           var    = n / (n - 1.0d0) * (meansq - mean * mean),
           sdev   = Math.sqrt(var) DO
        Debug.Out(F("mean %s sdev %s", LR(mean), LR(sdev)))
      END
    END
  END DoIt;

VAR
  pp := NEW(ParseParams.T).init(Stdio.stderr);
  ofn : Pathname.T := "hist.dat";
BEGIN
  TRY
    IF pp.keywordPresent("-K") THEN
      K := pp.getNextInt()
    END;

    IF pp.keywordPresent("-N") THEN
      N := pp.getNextInt()
    END;

    IF pp.keywordPresent("-H") THEN
      H := pp.getNextInt()
    END;

    IF pp.keywordPresent("-trunc") THEN
      Trunc := pp.getNextLongReal()
    END;

    IF pp.keywordPresent("-samples") THEN
      SamplesPerBin := pp.getNextInt()
    END;

    IF pp.keywordPresent("-gnpsigma") THEN
      WITH s = pp.getNextLongReal() DO
        Vssgnp.sigma :=  s;
        Vffgnp.sigma := -s
      END
    END;

    IF pp.keywordPresent("-Vss") THEN
      Vssgnp.v := pp.getNextLongReal()
    END;
    
    IF pp.keywordPresent("-Vff") THEN
      Vffgnp.v := pp.getNextLongReal()
    END;

    IF pp.keywordPresent("-o") THEN
      ofn := pp.getNext()
    END;
    
    pp.skipParsed()

  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command line")
  END;
  
  DoIt()
 
END Main.
