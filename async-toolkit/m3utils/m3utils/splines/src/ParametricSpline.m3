(* $Id$ *)

MODULE ParametricSpline;
IMPORT IntegrateTrap, DerivRidders;

REVEAL 
  T = Public BRANDED Brand OBJECT
  OVERRIDES 
    length := Length;
  END;

VAR
  mu := NEW(MUTEX);
  held := FALSE;
  intObj : T;

<* FATAL IntegrateTrap.NoConvergence *>

PROCEDURE IntFunc(x : LONGREAL) : LONGREAL =
  VAR
    dummy : LONGREAL;
    y := intObj.getParametricPoint(x).y;
    dx_ds := DerivRidders.Deriv(GetX, x, 1.0d0, dummy);
  BEGIN
    <* ASSERT held *>
    RETURN y * dx_ds
  END IntFunc;

PROCEDURE GetX(x : LONGREAL) : LONGREAL =
  BEGIN
    <* ASSERT held *>
    RETURN intObj.getParametricPoint(x).x
  END GetX;

(* $$\int_{s=0}^{s=1} y(s) {dx\over ds} \,ds$$ *)
PROCEDURE Length(self : T; minParam, maxParam : LONGREAL) : LONGREAL =

  PROCEDURE Integrate() : LONGREAL = 
    BEGIN
      RETURN IntegrateTrap.IntegrateE(IntFunc, minParam, maxParam)
    END Integrate;
    
  BEGIN
    LOCK mu DO
      intObj := self;
      <* ASSERT NOT held *>
      held := TRUE;
      TRY
        RETURN Integrate()
      FINALLY
        intObj := NIL;
        <* ASSERT held *>
        held := FALSE
      END
    END
  END Length;

BEGIN END ParametricSpline.
