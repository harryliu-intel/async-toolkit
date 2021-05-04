INTERFACE Sortable;

TYPE
  CompRes = [ -1 .. +1 ];
  
  T = OBJECT METHODS
    compare(with : T) : CompRes;
  END;

CONST Brand = "Sortable";

PROCEDURE Equal(a, b : T) : BOOLEAN; (* pointer comparison *)

PROCEDURE Compare(a, b : T) : CompRes; (* calls a.compare(b) *)

END Sortable.
