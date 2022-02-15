INTERFACE Edge;

TYPE
  T = RECORD
    from, to : TEXT;
  END;

CONST Brand = "Edge";

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

END Edge.
