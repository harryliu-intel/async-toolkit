INTERFACE RegC;
IMPORT RegCompiler;
IMPORT GenViews;
IMPORT RegComponent, RegGenState;
IMPORT GenCUtils;

CONST Brand = "RegC";

TYPE Public = RegCompiler.T;
     T      <: Public;
     Gen    = GenViews.T;

TYPE Phase = { P };

CONST
  PhaseNames = ARRAY Phase OF TEXT { "Gen" };

CONST ComponentTypeName = GenCUtils.ComponentTypeName;
  
END RegC.
