INTERFACE Triangle3;
IMPORT P3;

TYPE T = ARRAY [0..2] OF P3.T;

CONST Brand = "Triangle3";

CONST Equal : PROCEDURE(READONLY a, b : T) : BOOLEAN = NIL;

PROCEDURE MaxCos(READONLY t : T) : LONGREAL;

PROCEDURE Format(READONLY t : T; prec : CARDINAL := 3) : TEXT;

PROCEDURE FormatGnu(READONLY t : T) : TEXT;

PROCEDURE IntersectLine(lineOrigin    : P3.T;
                        lineDirection : P3.T;
                        VAR triangle  : T) : IntersectionResult;

TYPE
  IntersectionResult = RECORD
    intersect : BOOLEAN;
    N         : P3.T;
    t         : LONGREAL; (* distance along ray to intersection point *)
    u, v      : LONGREAL; (* barycentric coordinates of intersection point *)
  END;

END Triangle3.
