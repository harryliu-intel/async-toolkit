INTERFACE Interpolate;

PROCEDURE OverCard(READONLY x     : ARRAY OF CARDINAL;
                   READONLY y     : ARRAY OF LONGREAL;
                   READONLY meas  : CARDINAL) : LONGREAL;

PROCEDURE OverLR(READONLY x     : ARRAY OF LONGREAL;
                 READONLY y     : ARRAY OF LONGREAL;
                 READONLY meas  : LONGREAL) : LONGREAL;

PROCEDURE Exp(READONLY x     : ARRAY OF LONGREAL;
              READONLY y     : ARRAY OF LONGREAL;
              READONLY meas  : LONGREAL) : LONGREAL;
  (* exponential interpolation *)

CONST Brand = "Interpolate";

END Interpolate.
