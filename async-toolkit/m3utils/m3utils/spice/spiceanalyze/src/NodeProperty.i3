INTERFACE NodeProperty;

TYPE
  T = { IsNfetSourceDrain,
        IsPfetSourceDrain,
        IsNfetBody,
        IsPfetBody,
        IsNfetGate,
        IsPfetGate,
        IsVdd,
        IsGnd,
        IsCellBoundaryNode,
        IsDiodeCathode,
        IsDiodeAnode
  };

CONST Brand = "NodeProperty";

END NodeProperty.
