(* $Id$ *)

MODULE Spline;
IMPORT DerivRidders;

REVEAL 
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    deriv := Deriv
  END;

VAR
  curObj : T;
  mu := NEW(MUTEX);
  
PROCEDURE DummyFunc(at : LONGREAL) : LONGREAL =
  BEGIN RETURN curObj.eval(at) END DummyFunc;

PROCEDURE Deriv(self : T; at : LONGREAL) : LONGREAL =
  VAR
    dummyErr : LONGREAL;
  BEGIN
    LOCK mu DO
      curObj := self;
      (* 1.0d0 is just a guess. *)
      RETURN DerivRidders.Deriv(DummyFunc, at, 1.0d0, dummyErr)
    END
  END Deriv;
  
BEGIN END Spline.
