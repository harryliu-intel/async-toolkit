INTERFACE RdlPostPropertyAssign;
IMPORT RdlInstanceRef;
IMPORT RdlPropertyAssignRhs;

TYPE
  T = OBJECT
    instanceRef : RdlInstanceRef.T;
    rhs         : RdlPropertyAssignRhs.T;
  END;

CONST Brand = "RdlPostPropertyAssign";
      
END RdlPostPropertyAssign.
