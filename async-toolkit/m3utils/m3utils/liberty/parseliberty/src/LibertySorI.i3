INTERFACE LibertySorI;
IMPORT LibertyComponent;

TYPE
  T <: LibertyComponent.T;

  String = T BRANDED Brand & " String" OBJECT
    val : TEXT;
  END;

  Ident = T BRANDED Brand & " Ident" OBJECT
    val : TEXT;
  END;

CONST Brand = "LibertySorI";

END LibertySorI.
