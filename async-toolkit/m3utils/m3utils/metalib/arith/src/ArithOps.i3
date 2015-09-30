INTERFACE ArithOps;
IMPORT ArithR;

TYPE R = ArithR.T;

TYPE RealFunction = OBJECT METHODS f(x : LONGREAL) : LONGREAL; END;

PROCEDURE Plus(a, b : R) : R;

END ArithOps.

