INTERFACE DecoratedComponentDef;
IMPORT RdlComponentDef;
IMPORT RegComponent;

TYPE
  T <: Public;

  Public = RdlComponentDef.T OBJECT
    comp : RegComponent.T;
  METHODS
    init(old : RdlComponentDef.T; comp : RegComponent.T) : T;
  END;

CONST Brand = "DecoratedComponentDef";

END DecoratedComponentDef.
