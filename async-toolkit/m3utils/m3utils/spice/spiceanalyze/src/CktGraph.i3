INTERFACE CktGraph;
IMPORT CktElement;
IMPORT CktNode;
IMPORT SpiceObject;
IMPORT CktElementList;
IMPORT TextSet;
IMPORT CktNodeSeq;
IMPORT NodePropertySet;
IMPORT ElementPropertySet;

TYPE
  Mark = BRANDED OBJECT END; (* DFS mark *)

REVEAL
  CktElement.T = BRANDED OBJECT
    id        : INTEGER;
    src       : SpiceObject.T;
    terminals : CktNodeSeq.T;
    props     := ElementPropertySet.Empty;
    mark      : Mark := NIL;
  END;

  CktNode.T = BRANDED OBJECT
    id       : INTEGER;

    elements : CktElementList.T;
    (* note that one Element can be connected more than once to a Node *)
    
    aliases  : TextSet.T;
    props    := NodePropertySet.Empty;
    mark     : Mark := NIL;
  END;

TYPE Element = CktElement.T;
     Node    = CktNode.T;
       
CONST Brand = "CktGraph";

END CktGraph.
