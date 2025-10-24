(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE Main;

IMPORT ConjGradient;
IMPORT CSVParse;
IMPORT Measurement, MeasurementSeq;
IMPORT FileRd;
IMPORT Scan;
IMPORT Rd;
IMPORT Math;
FROM Fmt IMPORT FN, LongReal, Int, F;
IMPORT Fmt;
IMPORT FloatMode, Lex;
IMPORT IO;
IMPORT LRVector;
IMPORT LRScalarField;
IMPORT LRVectorField;
IMPORT Debug;

<*FATAL CSVParse.EndOfLine*>

CONST LR = LongReal;

VAR Pi := Math.atan(1.0d0) * 4.0d0;

PROCEDURE Func(theta, n : LONGREAL) : LONGREAL =
  BEGIN
    RETURN Math.pow(Math.sin(theta / 180.0d0 * Pi), n)
  END Func;

PROCEDURE Pihat(fi, thetai, A0, A1, B, n : LONGREAL) : LONGREAL =
  (* the model *)
  BEGIN
    RETURN A0 + A1 * fi * (1.0d0 + B * Func(thetai, n))
  END Pihat;

PROCEDURE DeltaPi(Pi, fi, thetai, A0, A1, B, n : LONGREAL) : LONGREAL =
  (* the error *)
  BEGIN
    RETURN Pihat(fi, thetai, A0, A1, B, n) - Pi
  END DeltaPi;

PROCEDURE Ei(Pi, fi, thetai, A0, A1, B, n : LONGREAL) : LONGREAL =
  (* the squared error -- our target function *)
  BEGIN
    WITH dP = DeltaPi(Pi, fi, thetai, A0, A1, B, n) DO
      RETURN dP * dP
    END
  END Ei;

PROCEDURE dEdA0i(Pi, fi, thetai, A0, A1, B, n : LONGREAL) : LONGREAL =
  (* first coordinate of the gradient *)
  BEGIN
    RETURN 2.0d0 * DeltaPi(Pi, fi, thetai, A0, A1, B, n)
  END dEdA0i;

PROCEDURE dEdA1i(Pi, fi, thetai, A0, A1, B, n : LONGREAL) : LONGREAL =
  (* second coordinate of the gradient *)
  BEGIN
    RETURN 2.0d0 * DeltaPi(Pi, fi, thetai, A0, A1, B, n) *
            fi * (1.0d0 + B * Func(thetai, n))
  END dEdA1i;

PROCEDURE dEdBi(Pi, fi, thetai, A0, A1, B, n : LONGREAL) : LONGREAL =
  (* third coordinate of the gradient *)
  BEGIN
    IF n = 0.0d0 THEN RETURN 0.0d0 END;
    RETURN 2.0d0 * DeltaPi(Pi, fi, thetai, A0, A1, B, n) *
            fi * A1 * Func(thetai, n)
  END dEdBi;

  (**********************************************************************)

  (* minimization routines *)

TYPE
  MinimizeFunc = LRScalarField.T OBJECT
    n            : LONGREAL;
    measurements : MeasurementSeq.T;
  OVERRIDES
    eval := EvalSqError;
  END;

PROCEDURE EvalSqError(mf : MinimizeFunc; vec : LRVector.T) : LONGREAL =
  VAR
    sum := 0.0d0;
  BEGIN
    FOR i := 0 TO mf.measurements.size() - 1 DO
      WITH mi    = mf.measurements.get(i),
           A0    = vec[0],
           A1    = vec[1],
           B     = vec[2],
           thisE = Ei(mi.P, mi.f, mi.theta, A0, A1, B, mf.n) DO
        sum := sum + thisE
      END
    END;
    RETURN sum
  END EvalSqError;

TYPE
  MinimizeGradient = LRVectorField.T OBJECT
    n            : LONGREAL;
    measurements : MeasurementSeq.T;
  OVERRIDES
    eval := EvalSqErrorGradient;
  END;

PROCEDURE EvalSqErrorGradient(mf  : MinimizeGradient;
                              vec : LRVector.T) : LRVector.T =
  VAR
    res := NEW(LRVector.T, NUMBER(vec^));
  BEGIN
    FOR i := FIRST(res^) TO LAST(res^) DO
      res[i] := 0.0d0
    END;
    
    FOR i := 0 TO mf.measurements.size() - 1 DO
      WITH mi    = mf.measurements.get(i),
           A0    = vec[0],
           A1    = vec[1],
           B     = vec[2],
           thisdEdA0 = dEdA0i(mi.P, mi.f, mi.theta, A0, A1, B, mf.n),
           thisdEdA1 = dEdA1i(mi.P, mi.f, mi.theta, A0, A1, B, mf.n),
           thisdEdB  = dEdBi (mi.P, mi.f, mi.theta, A0, A1, B, mf.n) DO
        res[0] := res[0] + thisdEdA0;
        res[1] := res[1] + thisdEdA1;
        res[2] := res[2] + thisdEdB;
      END
    END;
    RETURN res
  END EvalSqErrorGradient;
  
TYPE
  SampleIdx = [ 1 .. 9 ];
  AT        = ARRAY OF TEXT;

CONST Ftol = 1.0d-4;

PROCEDURE LR4(x : LONGREAL) : TEXT =
  BEGIN
    RETURN Fmt.LongReal(x, prec := 4, style := Fmt.Style.Fix)
  END LR4;
  
VAR
  sample : ARRAY SampleIdx OF MeasurementSeq.T;
  p := NEW(LRVector.T, 3);
BEGIN
  (***  PROGRAM STARTS HERE  ***)

  (* create sample data structures *)
  FOR i := FIRST(sample) TO LAST(sample) DO
    sample[i] := NEW(MeasurementSeq.T).init()
  END;


  (* parse data from file *)
  
  PROCEDURE ScanLR(txt : TEXT) : LONGREAL =
    (* helper routine to read a number and signal an error if something 
       is wrong *)
    BEGIN
      TRY
        RETURN Scan.LongReal(txt)
      EXCEPT
        Lex.Error, FloatMode.Trap =>
        Debug.Error(F("Couldn't parse LONGREAL %s on line %s", txt, Int(lNo)));
        <*ASSERT FALSE*>
      END
    END ScanLR;
    
  VAR
    rd := FileRd.Open("BevisData.csv");
    csv := NEW(CSVParse.T).init(rd);
    P, f, theta : LONGREAL;
    Ptxt : TEXT;
    s : CARDINAL;
    lNo := 1;
  BEGIN
    TRY
      LOOP
        csv.startLine();
        s := FIRST(SampleIdx);
        theta := ScanLR(csv.cell());
        WHILE csv.cellB(Ptxt) DO
          P := ScanLR(Ptxt);
          f := ScanLR(csv.cell());

          Debug.Out(F("Adding measurement %s { theta=%s, f=%s, P=%s }",
                      Int(s), LR(theta), LR(f), LR(P)));
          
          sample[s].addhi(Measurement.T { theta, f, P });
          
          INC(s)
        END;
        INC(lNo)
      END
    EXCEPT
      Rd.EndOfFile => Rd.Close(rd)
    END
  END;
  
  (* data have been loaded *)

  WITH fmt = "%8s %3s %10s %10s %10s %10s\n" DO
    IO.Put(FN(fmt, AT { "sample", "n", "A0", "A1", "B", "error"}));
    FOR si := FIRST(SampleIdx) TO LAST(SampleIdx) DO
      FOR i := FIRST(p^) TO LAST(p^) DO
        p[i] := 0.0d0
      END;
      FOR ni := 0 TO 2 DO
        WITH n   = FLOAT(ni, LONGREAL) DO

          (* iterate over all the samples *)
          
          IF ni = 0 THEN p[2] := 0.0d0 END;
          (* ugly, B is to be held 0 for n=0 *)
          
          WITH f     = NEW(MinimizeFunc,
                           n := n,
                           measurements := sample[si]),
               grad  = NEW(MinimizeGradient,
                           n := n,
                           measurements := sample[si]),
               sumSq = ConjGradient.Minimize(p, Ftol, f, grad) DO
            IO.Put(FN(fmt, AT { Int(si), Int(ni), LR4(p[0]), LR4(p[1]), LR4(p[2]), LR4(sumSq) }));

          END
        END
      END
    END
  END
END Main.
