GENERIC INTERFACE Node(Type);

TYPE
  Ref = REF T;
  
  T = RECORD
    nm   : TEXT;
    data : Type.T;
  END;

CONST Brand = "Node(" & Type.Brand & ")";
      
END Node.
