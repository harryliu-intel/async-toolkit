INTERFACE RdlComponentDefElem;
IMPORT RdlComponentDef;
IMPORT RdlEnumDef;
IMPORT RdlExplicitComponentInst;
IMPORT RdlPropertyAssign;

TYPE
  T = ROOT BRANDED OBJECT END;

  ComponentDef = T BRANDED OBJECT
    componentDef : RdlComponentDef.T;
  END;

  ComponentInst = T BRANDED OBJECT
    componentInst : RdlExplicitComponentInst.T;
  END;

  PropertyAssign = T BRANDED OBJECT
    propertyAssign : RdlPropertyAssign.T;
  END;

  EnumDef = T BRANDED OBJECT
    enumDef : RdlEnumDef.T;
  END;

CONST Brand = "RdlComponentDefElem";

      Equal : PROCEDURE (a, b : T) : BOOLEAN = NIL;

END RdlComponentDefElem.
     
