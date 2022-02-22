INTERFACE LibertySorI;
IMPORT LibertyComponent;

TYPE
  T <: LibertyComponent.T;

  String = T BRANDED OBJECT
    val : TEXT;
  END;

  Ident = T BRANDED OBJECT
    val : TEXT;
  END;

CONST Brand = "LibertySorI";

END LibertySorI.
