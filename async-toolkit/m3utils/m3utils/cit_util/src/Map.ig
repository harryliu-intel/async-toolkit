(* $Id$ *)

GENERIC INTERFACE Map(From,To);
IMPORT Word;
(* A Map.T is a wrapper for a 
   PROCEDURE (x : From.T) : To.T;

   It allows such functions to be dynamic (i.e., have internal state).
   A simple example would be a function that interpolates an array of
   values provided at runtime.
 *)

TYPE

  T <: Public;

  Public = OBJECT METHODS
    eval(x : From.T) : To.T; (* must override this *)

    evalD(x : From.T; VAR y : To.T); (* may override this if desired;
                                        inefficient default impl. is
                                        provided *)

    hash() : Word.T; (* may override this if desired *)
  END;

  (* sometimes all you need is a simple thing;
     in that case, you can use
     
     ft := NEW(Default).wrap(F)
  *)
  F = PROCEDURE (x : From.T) : To.T;

  Default <: T OBJECT METHODS
    wrap(f : F) : T;  
  END;

CONST Brand = "Map from " & From.Brand & " to " & To.Brand;

PROCEDURE Hash(a : T) : Word.T; (* calls hash method *)

PROCEDURE Equal(a, b : T) : BOOLEAN;

END Map.
