INTERFACE CspTypePublic;
IMPORT CspType;
IMPORT CspStructMemberSeq;
IMPORT CspExpression;
IMPORT CspInterval;

REVEAL
  CspType.ChannelStructure = CspType.T BRANDED CspType.Brand & " ChannelStructure" OBJECT
    members : CspStructMemberSeq.T;
  END;

  CspType.Integer <: CspType.T OBJECT
    isConst, isSigned : BOOLEAN;
    dw                : CspExpression.T;
    hasInterval       : BOOLEAN;
    interval          : CspInterval.T;
  END;
  
END CspTypePublic.
