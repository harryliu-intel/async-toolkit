INTERFACE RegContainer;
IMPORT RegComponent;
IMPORT RegChildSeq;

CONST
  Unspecified = LAST(CARDINAL);

TYPE
  T = RegComponent.T OBJECT
    children : RegChildSeq.T;
  END;

CONST Brand = "RegContainer";

END RegContainer.
