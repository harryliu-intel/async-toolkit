INTERFACE RdlInstanceRef;
IMPORT RdlInstanceRefElemSeq;
IMPORT RdlProperty;

TYPE
  T = OBJECT
    dotted : RdlInstanceRefElemSeq.T;
    property : RdlProperty.T;
  END;

CONST Brand = "RdlInstanceRef";

END RdlInstanceRef.
