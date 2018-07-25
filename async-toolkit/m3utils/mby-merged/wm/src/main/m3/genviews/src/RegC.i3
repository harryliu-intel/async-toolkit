INTERFACE RegC;
IMPORT RegCompiler;

CONST Brand = "RegC";

TYPE Public = RegCompiler.T;
     T      <: Public;

TYPE Phase = { P };

CONST
  PhaseNames = ARRAY Phase OF TEXT { "Gen" };
  
END RegC.
