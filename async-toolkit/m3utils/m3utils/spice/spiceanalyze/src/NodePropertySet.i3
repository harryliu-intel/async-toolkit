INTERFACE NodePropertySet;
IMPORT NodeProperty;

TYPE P = NodeProperty.T;
     
TYPE T = SET OF P;

CONST Brand = "NodePropertySet";

CONST
  Empty        = T {};
  NfetTerminal = T { P.IsNfetSourceDrain, P.IsNfetBody, P.IsNfetGate };
  PfetTerminal = T { P.IsPfetSourceDrain, P.IsPfetBody, P.IsPfetGate };
  
END NodePropertySet.
