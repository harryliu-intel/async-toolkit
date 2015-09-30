INTERFACE TimingModel;
IMPORT TransitionList, Name, ArithP;

TYPE
  T = OBJECT METHODS
    transitionTime(of       : Name.T;
                   newValue : BOOLEAN;
                   fanins   : TransitionList.T;
                   minMult  : LONGREAL) : ArithP.T;
  END;

END TimingModel.
