(* $Id$ *)

INTERFACE Example2;

TYPE 
  T = OBJECT METHODS
    xyz(x : LONGREAL := LAST(LONGREAL));

    uvw(x : INTEGER := FIRST(INTEGER)+1);
  END;

END Example2.
