(* $Id$ *)

GENERIC MODULE Map(From, To);
IMPORT Word;

REVEAL 
  T = Public BRANDED Brand OBJECT OVERRIDES 
    hash := THash;
    evalD := TEvalD;
  END;

TYPE
  PublicDef = T OBJECT METHODS wrap(f : F) : T END;

REVEAL 
  Default = PublicDef BRANDED "Default " & Brand OBJECT 
    f : F := NIL;
  OVERRIDES
    wrap := Wrap;  
    eval := DEval;
    evalHint := EvalHint;
  END;

PROCEDURE THash(<*UNUSED*>a : T) : Word.T = BEGIN RETURN 1 END THash;

PROCEDURE Hash(a : T) : Word.T =            BEGIN RETURN a.hash() END Hash;

PROCEDURE Equal(a,b : T) : BOOLEAN =        BEGIN RETURN a = b END Equal;

PROCEDURE Wrap(a : Default; f : F) : T =
  BEGIN 
    <* ASSERT a.f = NIL *>
    a.f := f;
    RETURN a
  END Wrap;

PROCEDURE EvalHint(<*UNUSED*>a : Default; <*UNUSED*>x : From.T) = 
  BEGIN END EvalHint;

PROCEDURE DEval(a : Default; x : From.T) : To.T =
  BEGIN RETURN a.f(x) END DEval;

(* The following is an inefficient, but correct, implementation of
   by-reference-return evaluation.

   An efficient implementation would do the evaluation in-place in y,
   destroying y's previous contents. *)

PROCEDURE TEvalD(self : T; x : From.T; VAR y : To.T) =
  VAR
    yy := self.eval(x);
  BEGIN
    y := yy
  END TEvalD;

BEGIN END Map.
