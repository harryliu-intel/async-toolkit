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

    (* evalHint provides a rudimentary way to parallelize the evaluation
       of slow functions.
       
       A client may call evalHint to indicate that it intends to evaluate
       the map in the future.  An implementation may use this information
       to evaluate the map early and memoize it, so that it knows the
       answer when the eval call comes.  The eval call is not guaranteed
       to occur, so the implementation should be careful not to store
       too much information. 
       
       The default implementation of evalHint is a No-op
    *)
    evalHint(x : From.T);

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
