(* $Id$ *)

GENERIC MODULE Map(From, To);
IMPORT Word;

REVEAL 
  T = Public BRANDED Brand OBJECT OVERRIDES hash := THash END;

TYPE
  PublicDef = T OBJECT METHODS wrap(f : F) : T END;

REVEAL 
  Default = PublicDef BRANDED "Default " & Brand OBJECT 
    f : F := NIL;
  METHODS
    wrap(f : PROCEDURE (x : From.T) : To.T) : T := Wrap;  
  OVERRIDES
    eval := DEval;
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

PROCEDURE DEval(a : Default; x : From.T) : To.T =
  BEGIN RETURN a.f(x) END DEval;
 
BEGIN END Map.
