INTERFACE RdlPropertyAssignClass;
FROM RdlPropertyAssign IMPORT T;
IMPORT RdlExplicitPropertyAssign;
IMPORT RdlPostPropertyAssign;

TYPE
  Explicit = T BRANDED OBJECT
    x : RdlExplicitPropertyAssign.T;
  END;

  Post = T BRANDED OBJECT
    x : RdlPostPropertyAssign.T;
  END;

END RdlPropertyAssignClass.
