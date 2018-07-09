INTERFACE ModelStageResult;
IMPORT ServerPacket AS Pkt;
IMPORT Metadata;

TYPE
  T = OBJECT
    opkt  : Pkt.T;
    om    : Metadata.T;
  END;

CONST Brand = "ModelStageResult";

END ModelStageResult.
