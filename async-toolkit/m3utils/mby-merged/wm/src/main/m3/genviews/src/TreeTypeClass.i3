INTERFACE TreeTypeClass;
IMPORT TreeType;
IMPORT TreeTypeSeq;
IMPORT RegComponentTypeTbl;
IMPORT RegComponent;

REVEAL
  TreeType.Struct = TreeType.T BRANDED OBJECT
    fields : TreeTypeSeq.T;
  END;

PROCEDURE To(c : RegComponent.T; tbl : RegComponentTypeTbl.T) : TreeType.T;
      
END TreeTypeClass.
