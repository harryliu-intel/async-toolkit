(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE ParametricSpectrum;
IMPORT LRFunction;
IMPORT LRVector;
IMPORT Math;
IMPORT SchemePair;
IMPORT SchemeLongReal;
IMPORT Blackbody;

REVEAL
  T = BRANDED OBJECT
    l0, l1       : LONGREAL;
    step         : LONGREAL;
    nparams      : CARDINAL;
    baseSpectrum : LRFunction.T;
  END;

PROCEDURE ZeroP(t : T) : LRVector.T =
  BEGIN
    WITH res = NEW(LRVector.T, t.nparams) DO
      FOR i := FIRST(res^) TO LAST(res^) DO
        res[i] := 0.0d0
      END;
      RETURN res
    END
  END ZeroP;

TYPE L = LONGREAL;
     
PROCEDURE New(l0, l1          : LONGREAL; (* lo, hi wavelengths *)

              nparams         : CARDINAL;

              baseSpectrum    : LRFunction.T
              (* the base spectrum to modify *)
  ) : T =
  BEGIN
    RETURN NEW(T,
               l0 := l0,
               l1 := l1,
               step := (l1 - l0) / FLOAT(nparams - 1, L),
               nparams := nparams,
               baseSpectrum := baseSpectrum)
  END New;


VAR pi      := 4.0d0 * Math.atan(1.0d0);
    sqrt2pi := Math.sqrt(2.0d0 * pi);

<*UNUSED*>
PROCEDURE Gaussian(mu, sigma, x : LONGREAL) : LONGREAL =
  BEGIN
    WITH s = (mu - x) / sigma DO
      RETURN 1.0d0 / (sigma / sqrt2pi) * Math.exp(-0.5d0 * s * s)
    END
  END Gaussian;
  
TYPE
  Function = LRFunction.T BRANDED OBJECT
    t : T;
    p : LRVector.T;
  OVERRIDES
    eval := EvalFunction;
  END;
    
PROCEDURE GetFunc(t : T; p : LRVector.T) : LRFunction.T =
  BEGIN
    RETURN NEW(Function, t := t, p := p)
  END GetFunc;

PROCEDURE EvalFunction(f : Function; l : LONGREAL) : LONGREAL =
  VAR
    lo : INTEGER;
  BEGIN
    <*ASSERT f # NIL*>
    <*ASSERT f.t # NIL*>
    <*ASSERT f.p # NIL*>
    <*ASSERT f.t.baseSpectrum # NIL*>

    lo := FLOOR((l - f.t.l0) / f.t.step);
    
    IF    lo <  0 THEN
      (* single-sided, step 0 *)
      RETURN Math.exp(f.p[0]) * f.t.baseSpectrum.eval(l)
    ELSIF lo >= f.t.nparams - 1 THEN
      (* single-sided, step nparams - 1 *)
      RETURN Math.exp(f.p[f.t.nparams - 1]) * f.t.baseSpectrum.eval(l)
    ELSE
      WITH hi  = lo + 1,
           lox = f.t.l0 + FLOAT(lo, L) * f.t.step,
           hix = lox + f.t.step,

           dx0 = l - lox,
           hr  = dx0 / f.t.step,
           lr  = 1.0d0 - hr,

           lop = f.p[lo],
           hip = f.p[hi],

           interp = lr * lop + hr * hip DO
        RETURN Math.exp(interp) * f.t.baseSpectrum.eval(l)
      END
    END
  END EvalFunction;

PROCEDURE ModifyV(v : LRVector.T; idx : CARDINAL; to : LONGREAL) : LONGREAL =
  VAR
    old := v[idx];
  BEGIN
    v[idx] := to;
    RETURN old
  END ModifyV;

PROCEDURE Scheme2Vec(lst : SchemePair.T) : LRVector.T =
  VAR
    p : SchemePair.T := lst;
    len := 0;
  BEGIN
    WHILE p # NIL DO
      INC(len);
      p := p.rest
    END;
    WITH res = NEW(LRVector.T, len) DO
      p := lst;
      FOR i := FIRST(res^) TO LAST(res^) DO
        res[i] := NARROW(p.first, SchemeLongReal.T)^;
        p := p.rest
      END;
      RETURN res
    END
  END Scheme2Vec;

PROCEDURE Vec2Scheme(vec : LRVector.T) : SchemePair.T =
  VAR
    res : SchemePair.T := NIL;
  BEGIN
    FOR i := LAST(vec^) TO FIRST(vec^) BY -1 DO
      res := NEW(SchemePair.T,
                 first := SchemeLongReal.FromLR(vec[i]),
                 rest  := res)
    END;
    RETURN res
  END Vec2Scheme;
  
PROCEDURE SetVec(to, from : LRVector.T) =
  BEGIN
    to^ := from^
  END SetVec;

PROCEDURE Subdivide(a : LRVector.T) : LRVector.T =
  VAR
    res : LRVector.T;
  BEGIN
    res := NEW(LRVector.T, NUMBER(a^) * 2 - 1);

    FOR i := FIRST(a^) TO LAST(a^) DO
      res[2 * i] := a[i]
    END;
    FOR j := 1 TO LAST(res^) - 1 BY 2 DO
      res[j] := 0.5d0 * (res[j - 1] + res[j + 1])
    END;
    RETURN res
  END Subdivide;

  

TYPE
  TruncBlackbody = LRFunction.T OBJECT
    temp, lambda0, lambda1 : LONGREAL;
  OVERRIDES
    eval := EvalTB;
  END;

PROCEDURE EvalTB(tb : TruncBlackbody; lambda : LONGREAL) : LONGREAL =
  CONST
    c = 299792458.0d0;
  BEGIN
    WITH nu = c / lambda DO
      IF    lambda < tb.lambda0 THEN
        RETURN 0.0d0
      ELSIF lambda > tb.lambda1 THEN
        RETURN 0.0d0
      ELSE                           
        RETURN Blackbody.PlanckRadiance(tb.temp, nu) / c / nu / nu
      END
    END
  END EvalTB;
        
PROCEDURE MakeBlackbodyInWavelength(temp, lambda0, lambda1 : LONGREAL) : LRFunction.T =
  BEGIN
    RETURN NEW(TruncBlackbody,
               temp := temp, lambda0 := lambda0, lambda1 := lambda1)
  END MakeBlackbodyInWavelength;
  

BEGIN END ParametricSpectrum.
