INTERFACE ModelStageResultClass;
IMPORT ModelStageResult;
IMPORT ServerPacket AS Pkt;
IMPORT Metadata;
IMPORT Coroutine;

REVEAL
  ModelStageResult.T <: Private;

TYPE
  Private = ModelStageResult.Public OBJECT
    opkt  : Pkt.T;
    om    : Metadata.T;
    co    : Coroutine.T;
  METHODS
    init(co : Coroutine.T) : ModelStageResult.T;
  END;

END ModelStageResultClass.
