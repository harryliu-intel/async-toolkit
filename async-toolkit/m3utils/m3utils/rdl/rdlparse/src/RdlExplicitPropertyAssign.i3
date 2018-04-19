INTERFACE RdlExplicitPropertyAssign;
IMPORT RdlProperty;
IMPORT RdlPropertyAssignRhs;
IMPORT RdlPropertyModifier;

TYPE
  T = OBJECT
    haveModifier : BOOLEAN;
    modifier     : RdlPropertyModifier.T;
    default      : BOOLEAN;
    property     : RdlProperty.T;
    rhs          : RdlPropertyAssignRhs.T;
  END;

CONST Brand = "RdlExplicitPropertyAssign";

PROCEDURE Format(t : T) : TEXT;

END RdlExplicitPropertyAssign.
