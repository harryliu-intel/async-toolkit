INTERFACE CspDirection;

TYPE
  T = { None, In, Out, InOut };

CONST
  Names = ARRAY T OF TEXT {
  "none",
  "in",
  "out",
  "inout"
  };
  
CONST Brand = "CspDirection";

END CspDirection.
