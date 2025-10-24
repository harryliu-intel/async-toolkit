GENERIC INTERFACE Node(Type);
IMPORT CspNode;

TYPE
  Ref = T;
  
  T = CspNode.T OBJECT
    data : Type.T;
  END;

CONST Brand = "Node(" & Type.Brand & ")";
      
END Node.
