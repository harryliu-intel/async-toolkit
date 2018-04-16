INTERFACE RegReg;
IMPORT RegFieldSeq;
IMPORT RegComponent;

CONST
  Unspecified = LAST(CARDINAL);

TYPE
  T = RegComponent.T OBJECT
    fields : RegFieldSeq.T;
  END;

CONST
  Brand = "RegReg";

END RegReg.
