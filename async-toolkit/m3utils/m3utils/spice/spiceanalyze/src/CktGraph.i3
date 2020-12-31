INTERFACE CktGraph;
IMPORT CktElement;
IMPORT CktNode;
IMPORT SpiceObject;
IMPORT CktNodeList;
IMPORT CktElementList;
IMPORT TextSet;

TYPE
  Mark = BRANDED OBJECT END; (* DFS mark *)

REVEAL
  CktElement.T = BRANDED OBJECT
    src       : SpiceObject.T;
    terminals : CktNodeList.T;
    mark      : Mark;
  END;

  CktNode.T = BRANDED OBJECT
    elements : CktElementList.T;
    aliases  : TextSet.T;
    mark     : Mark;
  END;

TYPE Element = CktElement.T;
     Node    = CktNode.T;
       
CONST Brand = "CktGraph";

END CktGraph.
