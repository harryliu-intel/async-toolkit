INTERFACE TreeTypeStruct;
IMPORT TreeType;
IMPORT TreeTypeSeq;

REVEAL
  TreeType.Struct = TreeType.T BRANDED OBJECT
    fields : TreeTypeSeq.T;
  END;

CONST Brand = "TreeTypeStruct";

END TreeTypeStruct.
