INTERFACE RdlPropertyBody;
IMPORT RdlPropertyType, RdlPropertyComponentDisjunction, RdlPropertyDefault;

TYPE
  T = BRANDED Brand OBJECT
    type  : RdlPropertyType.T;
    usage : RdlPropertyComponentDisjunction.T;
    def   : RdlPropertyDefault.T;
  END;

CONST Brand = "RdlPropertyBody";

END RdlPropertyBody.
