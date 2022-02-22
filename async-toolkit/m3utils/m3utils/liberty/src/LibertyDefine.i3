INTERFACE LibertyDefine;
IMPORT LibertyComponent;
IMPORT LibertySorI;

TYPE
  T <: LibertyComponent.T;

  X = T OBJECT
    s : ARRAY [0..2] OF LibertySorI.T;
  END;

CONST Brand = "LibertyDefineGroup";

END LibertyDefine.
