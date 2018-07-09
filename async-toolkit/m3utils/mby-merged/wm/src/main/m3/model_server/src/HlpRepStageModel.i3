INTERFACE HlpRepStageModel;
IMPORT Coroutine;
IMPORT ServerPacket AS Pkt;
IMPORT hlp_top_map_addr AS TopAddr;
IMPORT Metadata;
IMPORT ModelStageResult;

TYPE
  Indices = RECORD
  END;

PROCEDURE HandlePacket(from    : Coroutine.T;
                       ipkt    : Pkt.T;
                       h       : TopAddr.H;
                       indices : Indices;
                       imd     : Metadata.T;
                       out     : ModelStageResult.T);

TYPE Meta = Metadata.T;

END HlpRepStageModel.
