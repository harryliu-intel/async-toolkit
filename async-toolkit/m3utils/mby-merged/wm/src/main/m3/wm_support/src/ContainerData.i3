INTERFACE ContainerData;
IMPORT AddrVisitor;

TYPE Neg = [ FIRST(INTEGER) .. -1 ];
     Pos = CARDINAL;

TYPE
  T =  AddrVisitor.Internal OBJECT
    id : Neg;
    up : T;
  END;

CONST Brand = "ContainerData";

END ContainerData.
