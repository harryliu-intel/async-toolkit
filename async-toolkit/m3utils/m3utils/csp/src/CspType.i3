INTERFACE CspType;
IMPORT CspSyntax;
IMPORT CspRange;
IMPORT CspDirection;
IMPORT CspInterval;
IMPORT BigInt;

TYPE
  T <: Public;

  Public = CspSyntax.T;

  PubArray = T OBJECT
    range      : CspRange.T;
    elemntType : T;
  END;

  Array <: PubArray;

  Boolean <: T;

  ChannelStructure <: T; (* see CspTypePublic.i3 *)

  PubChannel = T OBJECT
    numValues : BigInt.T;
    dir       : CspDirection.T;
  END;

  Channel <: PubChannel;

  PubInteger = T OBJECT
    isConst, isSigned : BOOLEAN;
    hasDw             : BOOLEAN;
    dw                : CARDINAL;
    hasInterval       : BOOLEAN;
    interval          : CspInterval.T;
  END;

  Integer <: PubInteger;

  PubNode = T OBJECT
    arrayed   : BOOLEAN;
    width     : [1..LAST(CARDINAL)];
    direction : CspDirection.T;
  END;

  Node <: PubNode;

  String <: T;
  
  PubStructure = T OBJECT
    isConst : BOOLEAN;
    name    : TEXT;
  END;

  Structure <: PubStructure;
  
CONST Brand = "CspType";

END CspType.
