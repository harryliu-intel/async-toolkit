MODULE Variation_p1274_3x2r1;
FROM Math IMPORT sqrt;
IMPORT Perturbation, PerturbationRep;
IMPORT Text;
IMPORT Variation;
FROM Fmt IMPORT F, LongReal;
IMPORT Debug;

CONST TE = Text.Equal;

TYPE Pert = { dell, xu0, gw, fms };

CONST PertNames = ARRAY Pert OF TEXT { "dell", "xu0", "gw", "fms" };

CONST Params = ARRAY TranType OF ARRAY Pert OF LONGREAL {
  ARRAY Pert OF LONGREAL { 0.0003246d0, -0.02271d0, 0.0d0, 0.00132d0   },
  ARRAY Pert OF LONGREAL { 0.0003246d0, -0.02271d0, 0.0d0, 0.00121d0   },
  ARRAY Pert OF LONGREAL { 0.0002651d0, -0.016d0  , 0.0d0, 0.0014478d0 },
  ARRAY Pert OF LONGREAL { 0.0003246d0, -0.02271d0, 0.0d0, 0.00132d0   },
  ARRAY Pert OF LONGREAL { 0.0002651d0, -0.016d0  , 0.0d0, 0.0014478d0 },
  ARRAY Pert OF LONGREAL { 0.0002651d0, -0.016d0  , 0.0d0, 0.0013738d0 },
  ARRAY Pert OF LONGREAL { 0.0003246d0, -0.02271d0, 0.0d0, 0.00132d0   },
  ARRAY Pert OF LONGREAL { 0.0003246d0, -0.02271d0, 0.0d0, 0.00132d0   },
  ARRAY Pert OF LONGREAL { 0.0002651d0, -0.016d0  , 0.0d0, 0.0013738d0 },
  ARRAY Pert OF LONGREAL { 0.0003246d0, -0.02271d0, 0.0d0, 0.00132d0   },
  ARRAY Pert OF LONGREAL { 0.0002651d0, -0.016d0  , 0.0d0, 0.0012995d0 },
  ARRAY Pert OF LONGREAL { 0.0002651d0, -0.016d0  , 0.0d0, 0.0014478d0 }
  };

PROCEDURE ParsePert(nm : TEXT) : Pert =
  BEGIN
    FOR i := FIRST(Pert) TO LAST(Pert) DO
      IF TE(nm, PertNames[i]) THEN RETURN i END
    END;
    <*ASSERT FALSE*>
  END ParsePert;
      
PROCEDURE ParseType(nm : TEXT) : TranType =
  BEGIN
    FOR i := FIRST(TranType) TO LAST(TranType) DO
      IF TE(nm, TranTypeNames[i]) THEN RETURN i END
    END;
    <*ASSERT FALSE*>
  END ParseType;

PROCEDURE CalcPert(p : Perturbation.Default) : LONGREAL =
  (* collection routine for all of them *)
  VAR
    pert := ParsePert(p.var);
    type := ParseType(p.model);
    fudge := Params[type, pert];
  BEGIN
    WITH v = Calc[pert](p, pert, type, fudge) DO
      Debug.Out(F("Calc pert. %s of %s fudge %s value %s", p.var, p.model, LongReal(fudge), LongReal(v)));
      RETURN v
    END
  END CalcPert;

CONST Calc = ARRAY Pert OF PROCEDURE (p : Perturbation.Default; pert : Pert; type : TranType; fudge : LONGREAL) : LONGREAL
  { CalcDell, CalcXu0, CalcGw, CalcFms };

(* variations are vtsingle and lermat *)
      
PROCEDURE CalcFms(p : Perturbation.Default; pert : Pert; type : TranType; fudge : LONGREAL) : LONGREAL =
  VAR
    vtsingle := p.v("vtsingle");
  BEGIN
    RETURN (-1.0d0)*((vtsingle/sqrt(p.v("M")))*fudge/sqrt(((p.v("WdrawnUm")))*(p.v("gw")+p.v("gh")*2.0d0)/p.v("pitch")*(((p.v("LdrawnUm")))-2.0d0*p.v("dfl")+p.v("dell"))))
  END CalcFms;

PROCEDURE CalcXu0(p : Perturbation.Default; pert : Pert; type : TranType; fudge : LONGREAL) : LONGREAL =
  VAR
    lermat := p.v("lermat");
    pct := (fudge*(lermat/sqrt(p.v("M")))*sqrt(1.0d0/((p.v("WdrawnUm")))*p.v("pitch")/(p.v("gw")+p.v("gh")*2.0d0)))*100.0d0;
  BEGIN
    RETURN p.v("xu0")*(pct/100.0d0)
  END CalcXu0;
  
PROCEDURE CalcGw(p : Perturbation.Default; pert : Pert; type : TranType; fudge : LONGREAL) : LONGREAL =
  VAR
    lermat := p.v("lermat");
  BEGIN
    RETURN (-1.0d0)*1.0d-4*(lermat/sqrt(p.v("M")))*(0.00009d0+0.0000545d0*sqrt(1.0d0/(((p.v("WdrawnUm")))/MAX(p.v("NF"),1.0d0))*p.v("pitch")/(p.v("gw")+p.v("gh")*2.0d0)))/sqrt(MAX(p.v("NF"),1.0d0))
  END CalcGw;

PROCEDURE CalcDell(p : Perturbation.Default; pert : Pert; type : TranType; fudge : LONGREAL) : LONGREAL =
  VAR
    lermat := p.v("lermat");
    f1 := (lermat/sqrt(p.v("M")));
    t21 := 0.00044d0;
    fx0 := 1.0d0/(((p.v("WdrawnUm")))/MAX(p.v("NF"),1.0d0));
    fx1 := p.v("pitch")/(p.v("gw")+p.v("gh")*2.0d0);
    x := fx0 * fx1;
    t22 := fudge*sqrt(x);
    f2 := t21+t22;
    f3 := 1.0d0/sqrt(MAX(p.v("NF"),1.0d0)); 
    res := f1*f2*f3;

  BEGIN
(*
    Debug.Out(F("CalcDell lermat %s f1 %s t21 %s t22 %s f2 %s",
                LongReal(lermat), LongReal(f1), LongReal(t21), LongReal(t22),
                LongReal (f2)));
*)
    Debug.Out(F("CalcDell res %s", LongReal(res)));
    RETURN res
  END CalcDell;

PROCEDURE New() : Variation.T =
  VAR
    variation : Variation.Default := NEW(Variation.Default).init(HspName);
  BEGIN
    FOR pi := FIRST(Pert) TO LAST(Pert) DO
      FOR ti := FIRST(TranType) TO LAST(TranType) DO
        WITH p = NEW(Perturbation.Default, calc := CalcPert).init(TranTypeNames[ti], PertNames[pi]) DO
          variation.setPerturbation(p)
        END
      END
    END;
    RETURN variation
  END New;

BEGIN END Variation_p1274_3x2r1.
