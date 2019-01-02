INTERFACE RegCApi;
IMPORT RegCompiler;
IMPORT GenViews;
IMPORT GenCUtils;
IMPORT RegC;

CONST Brand = "RegCApi";

TYPE Public = RegC.Public;
     T      <: Public;
     Gen    = RegC.Gen;

TYPE Phase = { P };

CONST
  PhaseNames = RegC.PhaseNames;
  
CONST ComponentTypeName = RegC.ComponentTypeName;

END RegCApi.
