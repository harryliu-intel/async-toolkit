INTERFACE LibertyDefine;
IMPORT LibertyComponent;
IMPORT LibertySorI;

TYPE
  T <: Public;

  Public = LibertyComponent.T OBJECT
    s : ARRAY [0..2] OF LibertySorI.T;
  END;

CONST Brand = "LibertyDefine";

END LibertyDefine.
