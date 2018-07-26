INTERFACE Metadata;

TYPE
  T <: Public;

  Public = OBJECT
    next : T; (* allow the linking of multiple Ts *)
  METHODS
    ofType(tc : Typecode) : T RAISES { NonUnique };
  END;

  Typecode = CARDINAL;

EXCEPTION NonUnique; (* not exactly one instance of a given type in list *)
          
CONST Brand = "Metadata";

END Metadata.
