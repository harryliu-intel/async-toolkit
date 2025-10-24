MODULE X01;
IMPORT Bit;

PROCEDURE ToBit(t : T) : Bit.T =
  BEGIN
    CASE t OF
      T.V0 => RETURN 0
    |
      T.V1 => RETURN 1
    |
      T.VX => <*ASSERT FALSE*>
    END
  END ToBit;

PROCEDURE FromBit(b : Bit.T) : T =
  CONST 
    Tab = ARRAY Bit.T OF T { T.V0, T.V1 };
  BEGIN 
    RETURN Tab[b] 
  END FromBit;

BEGIN END X01.
