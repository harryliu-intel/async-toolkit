INTERFACE CspTypePublic;
IMPORT CspType;
IMPORT CspStructMemberSeq;

REVEAL
  CspType.ChannelStructure = CspType.T BRANDED CspType.Brand & " ChannelStructure" OBJECT
    members : CspStructMemberSeq.T;
  END;

END CspTypePublic.
