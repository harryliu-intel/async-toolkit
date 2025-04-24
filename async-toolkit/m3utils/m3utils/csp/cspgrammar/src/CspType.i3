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

  MayBeConst = T OBJECT
    isConst : BOOLEAN;
  END;
  
  Boolean <: MayBeConst;

  ChannelStructure <: T; (* see CspTypePublic.i3 *)

  PubChannel = T OBJECT
    numValues : BigInt.T;
    dir       : CspDirection.T;
  END;

  Channel <: PubChannel;

  Integer <: MayBeConst; (* see CspTypePublic.i3 *)

  PubNode = T OBJECT
    arrayed   : BOOLEAN;
    width     : [1..LAST(CARDINAL)];
    direction : CspDirection.T;
  END;

  Node <: PubNode;

  String <: MayBeConst;
  
  PubStructure = MayBeConst OBJECT
    name    : TEXT;
  END;

  Structure <: PubStructure;
  
CONST Brand = "CspType";

END CspType.
