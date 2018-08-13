GENERIC INTERFACE TopoSort(Elem, ElemSeq);

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init() : T;

    addDependency(pred, succ : Elem.T);

    sort() : ElemSeq.T;
  END;

CONST Brand = "TopoSort(" & Elem.Brand & ")";

END TopoSort.
    
