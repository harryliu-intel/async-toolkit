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

  Reverse = ARRAY T OF T {
  T.None,
  T.Out,
  T.In,
  T.InOut
  };
  
CONST Brand = "CspDirection";

END CspDirection.
