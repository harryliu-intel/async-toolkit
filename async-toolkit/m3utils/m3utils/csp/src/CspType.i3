INTERFACE CspType;
IMPORT CspSyntax;
IMPORT CspRange;
IMPORT CspDirection;
IMPORT CspInterval;
IMPORT BigInt;

TYPE
  T <: Public;

  Public = CspSyntax.T;

  Array = T BRANDED Brand & " Array" OBJECT
    range      : CspRange.T;
    elemntType : T;
  END;

  Boolean = T BRANDED Brand & " Boolean" OBJECT
  END;

  ChannelStructure <: T; (* see CspTypePublic.i3 *)

  Channel = T BRANDED Brand & " Channel" OBJECT
    numValues : BigInt.T;
    dir       : CspDirection.T;
  END;

  Integer = T BRANDED Brand & " Integer" OBJECT
    isConst, isSigned : BOOLEAN;
    dw                : CARDINAL;
    interval          : CspInterval.T;
  END;

  Node = T BRANDED Brand & " Node" OBJECT
    arrayed   : BOOLEAN;
    width     : [1..LAST(CARDINAL)];
    direction : CspDirection.T;
  END;

  String = T BRANDED Brand & " String" OBJECT
  END;

  Structure = T BRANDED Brand & " Structure" OBJECT
    isConst : BOOLEAN;
    name    : TEXT;
  END;
  
CONST Brand = "CspType";

END CspType.
