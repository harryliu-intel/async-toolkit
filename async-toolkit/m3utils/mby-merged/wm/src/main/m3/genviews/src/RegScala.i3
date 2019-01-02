INTERFACE RegScala;
IMPORT RegCompiler;
IMPORT GenViews;

CONST Brand = "RegScala";

TYPE Public = RegCompiler.T;
     T      <: Public;
     Gen    = GenViews.T;

TYPE Phase = { P };

CONST
  PhaseNames = ARRAY Phase OF TEXT { "Gen" };
  
END RegScala.
