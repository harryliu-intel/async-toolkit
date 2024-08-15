INTERFACE GenOpt;

PROCEDURE DefOptVar(nm : TEXT; defval, defstep : LONGREAL);

PROCEDURE SetRhoBeg(to : LONGREAL);

PROCEDURE SetRhoEnd(to : LONGREAL);

CONST Brand = "GenOpt";
  
END GenOpt.
