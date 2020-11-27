MODULE CardTriple;

PROCEDURE Add(READONLY a, b : T) : T =
  BEGIN
    RETURN T { a[0] + b[0], a[1] + b[1], a[2] + b[2] }
  END Add;

BEGIN END CardTriple.
    
