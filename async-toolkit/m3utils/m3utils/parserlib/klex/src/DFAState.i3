INTERFACE DFAState;
IMPORT DFATransList;
IMPORT NFAState;
CONST
  Brand = "DFAState";
TYPE
  T = REF RECORD
    next: DFATransList.T;
    ID: INTEGER;
    output: INTEGER;
    src: NFAState.T;
  END;

PROCEDURE Equal(a,b:T):BOOLEAN;

END DFAState.
