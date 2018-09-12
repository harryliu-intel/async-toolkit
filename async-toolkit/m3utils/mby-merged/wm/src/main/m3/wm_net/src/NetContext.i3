INTERFACE NetContext;

TYPE
  T = RECORD
    rem : CARDINAL; (* bytes remaining *)
  END;

CONST Brand = "NetContext";

EXCEPTION Short;
          
END NetContext.
