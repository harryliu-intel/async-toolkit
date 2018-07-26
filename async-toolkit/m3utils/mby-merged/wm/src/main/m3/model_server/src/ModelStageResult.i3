INTERFACE ModelStageResult;
IMPORT ServerPacket AS Pkt;
IMPORT Metadata;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    push(opkt : Pkt.T; om : Metadata.T);
  END;
  
CONST Brand = "ModelStageResult";

END ModelStageResult.
