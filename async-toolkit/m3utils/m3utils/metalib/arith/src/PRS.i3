INTERFACE PRS;
IMPORT Dsim, Name, RefList, ArithP;
IMPORT NameRefTbl;
IMPORT Transition, TransitionList;
IMPORT TimingModel;

TYPE
  XTransition = OBJECT  
    name     : Name.T;
    newValue : BOOLEAN; 
    pred     : TransitionList.T;
    at       : ArithP.T 
  END;

REVEAL Transition.T = XTransition BRANDED OBJECT END;

TYPE
  Node = OBJECT name : Name.T; transitions : TransitionList.T END;
  
  Rule = Dsim.Rule;

  PRS <: Public;

  Public = OBJECT
    nodes : NameRefTbl.T;
    rules : RefList.T;
  METHODS
    init(timingModel : TimingModel.T) : T;
    node(named : Name.T) : Node;
  END;

  T = PRS;

END PRS.
