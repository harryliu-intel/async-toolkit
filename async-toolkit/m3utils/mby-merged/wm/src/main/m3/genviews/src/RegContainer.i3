INTERFACE RegContainer;
IMPORT RegComponent;
IMPORT RegChildSeq;

CONST
  Unspecified = LAST(CARDINAL);

TYPE
  T <: Public;

  Public = RegComponent.T OBJECT
    children : RegChildSeq.T;
  METHODS
    skipArc() : BOOLEAN; (* skip my name, because I'm a nested array *)
  END;

CONST Brand = "RegContainer";

END RegContainer.
