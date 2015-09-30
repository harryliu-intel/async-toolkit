INTERFACE ArithP;
IMPORT Arith, ArithR;

TYPE R = ArithR.T;

TYPE T <: Arith.T;

PROCEDURE NewPair(x, y : R) : T; (* ordered pair *)

PROCEDURE GetX(p : T) : R;

PROCEDURE GetY(p : T) : R;

TYPE RArr = REF ARRAY OF R;
     TArr = REF ARRAY OF T;

PROCEDURE SelectMin(by  : RArr;
                    val : TArr) : T;

END ArithP.
