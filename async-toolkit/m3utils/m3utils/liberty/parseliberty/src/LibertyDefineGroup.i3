INTERFACE LibertyDefineGroup;
IMPORT LibertyComponent;
IMPORT LibertySorI;

TYPE
  T <: Public;

  Public = LibertyComponent.T OBJECT
    s : ARRAY [0..1] OF LibertySorI.T;
  END;

CONST Brand = "LibertyDefineGroup";

END LibertyDefineGroup.
