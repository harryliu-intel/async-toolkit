INTERFACE CktGraphDfs;
IMPORT CktGraph AS G;

PROCEDURE Node(n : G.Node; v : NodeVisitor);

TYPE
  NodeVisitor = OBJECT METHODS
    visit(prev : G.Node; via : G.Element; this : G.Node) : BOOLEAN;
    (* return whether you want to continue searching *)
  END;

CONST Brand = "CktGraphDfs";
      
END CktGraphDfs.
