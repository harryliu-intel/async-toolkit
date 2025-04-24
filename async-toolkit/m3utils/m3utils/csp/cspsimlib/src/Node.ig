GENERIC INTERFACE Node(Type);

TYPE
  T = RECORD
    nm   : TEXT;
    data : Type.T;
  END;

CONST Brand = "Node(" & Type.Brand & ")";
      
END Node.
