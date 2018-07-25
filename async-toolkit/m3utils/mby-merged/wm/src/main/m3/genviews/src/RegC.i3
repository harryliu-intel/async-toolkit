<<<<<<< HEAD
INTERFACE RegC;
IMPORT RegCompiler;

CONST Brand = "RegC";

TYPE Public = RegCompiler.T;
     T      <: Public;

TYPE Phase = { P };

CONST
  PhaseNames = ARRAY Phase OF TEXT { "Gen" };
  
END RegC.
||||||| parent of a0610bc... first C model
=======
INTERFACE RegC;
IMPORT RegCompiler;

CONST Brand = "RegScala";

TYPE Public = RegCompiler.T;
     T      <: Public;

TYPE Phase = { P };

CONST
  PhaseNames = ARRAY Phase OF TEXT { "Gen" };
  
END RegC.
>>>>>>> a0610bc... first C model
