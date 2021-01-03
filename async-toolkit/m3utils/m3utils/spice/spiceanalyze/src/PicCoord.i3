INTERFACE PicCoord;
IMPORT LongReal AS Parent;

TYPE T = Parent.T;

     NonNeg = T; (* no distinction for floating point *)
     
CONST Brand = "PicCoord(" & Parent.Brand & ")";

CONST Hash    = Parent.Hash;
      Equal   = Parent.Equal;
      Compare = Parent.Compare;

END PicCoord.
     
