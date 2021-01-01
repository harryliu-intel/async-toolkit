INTERFACE NodeProperty;

TYPE
  T = { IsNfetSourceOrDrain,
        IsPfetSourceOrDrain,
        IsSourceOrDrain,
        IsNfetBody,
        IsPfetBody,
        IsNfetTerminal,
        IsPfetTerminal,
        IsFetGate,
        IsVdd,
        IsGnd,
        IsCellBoundaryNode,
        IsDiodeCathode,
        IsDiodeAnode
  };

CONST Brand = "NodeProperty";

END NodeProperty.
