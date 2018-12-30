INTERFACE TreeTypeClass;
IMPORT TreeType;
IMPORT TreeTypeSeq;
IMPORT TreeTypeArraySeq;

REVEAL
  TreeType.Struct = TreeType.T BRANDED OBJECT
    fields : TreeTypeSeq.T;
  END;

PROCEDURE GetArrays(t : TreeType.T; seq : TreeTypeArraySeq.T);

END TreeTypeClass.
