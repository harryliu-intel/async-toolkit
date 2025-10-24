MODULE ArithP;
IMPORT ArithRep;
IMPORT Arith;

REVEAL T = Arith.T BRANDED OBJECT END;

PROCEDURE NewPair(x, y : R) : T =
  BEGIN RETURN NEW(ArithRep.PPair, x := x, y := y) END NewPair;

PROCEDURE GetX(p : T) : R =
  BEGIN
    TYPECASE p OF
      ArithRep.PPair(pp) => RETURN pp.x
    ELSE
      RETURN NEW(ArithRep.RFromPair, p := p, whch := ArithRep.XY.X)
    END
  END GetX;

PROCEDURE GetY(p : T) : R =
  BEGIN
    TYPECASE p OF
      ArithRep.PPair(pp) => RETURN pp.y
    ELSE
      RETURN NEW(ArithRep.RFromPair, p := p, whch := ArithRep.XY.Y)
    END
  END GetY;

PROCEDURE SelectMin(by  : REF ARRAY OF R; 
                    val : REF ARRAY OF T) : T =
  BEGIN
    <*ASSERT NUMBER(by^) = NUMBER(val^)*>
    <*ASSERT NUMBER(by^) > 0*>
    RETURN NEW(ArithRep.RSelectP, by := by, val := val)
  END SelectMin;

BEGIN END ArithP.
