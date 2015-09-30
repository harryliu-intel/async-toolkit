INTERFACE X01;
IMPORT Bit;

TYPE T = { VX, V0, V1 };

PROCEDURE ToBit(t : T) : Bit.T;

PROCEDURE FromBit(b : Bit.T) : T;

CONST Names = ARRAY T OF TEXT { "X", "0", "1" };

END X01.
