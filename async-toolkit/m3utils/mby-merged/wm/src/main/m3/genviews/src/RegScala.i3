INTERFACE RegScala;
IMPORT RegCompiler;

CONST Brand = "RegScala";

TYPE Public = RegCompiler.T;
     T      <: Public;

TYPE Phase = { P };

CONST
  PhaseNames = ARRAY Phase OF TEXT { "Gen" };
  
END RegScala.
