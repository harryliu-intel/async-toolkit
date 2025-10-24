INTERFACE PicCoord;
IMPORT LongReal AS Parent;
IMPORT Fmt;

TYPE T = Parent.T;

     NonNeg = T; (* no distinction for floating point *)
     
CONST Brand = "PicCoord(" & Parent.Brand & ")";

CONST Hash    = Parent.Hash;
      Equal   = Parent.Equal;
      Compare = Parent.Compare;

CONST Zero = 0.0d0;

CONST Format = Fmt.LongReal;

END PicCoord.
     
