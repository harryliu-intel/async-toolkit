MODULE MbyMapperStageModel;

IMPORT mby_top_map_addr AS TopAddr;

IMPORT mby_ppe_mapper_map AS Map;
IMPORT mby_ppe_mapper_map_addr AS MapAddr;

IMPORT ServerPacket AS Pkt;
IMPORT Metadata;
IMPORT MbyMapperToClassifierMeta;
IMPORT ModelStageResult;

PROCEDURE HandlePacket(ipkt : Pkt.T;
                       h : TopAddr.H;
                       indices : Indices;
                       imd : Metadata.T;
                       out : ModelStageResult.T) =
  BEGIN
    (* purpose of this routine is to map the records *)
    HandlePacketInt(ipkt,
                    h.read  .Mpt[indices.MptIdx].RxPpe.Mapper,
                    h.update.Mpt[indices.MptIdx].RxPpe.Mapper,
                    imd,
                    out)
  END HandlePacket;
  
PROCEDURE HandlePacketInt(ipkt        : Pkt.T;
                          READONLY r  : Map.T;
                          READONLY u  : MapAddr.U;
                          im          : Metadata.T;
                          out         : ModelStageResult.T) =
  BEGIN
    out.push(opkt := ipkt, om := NEW(MbyMapperToClassifierMeta.T))
  END HandlePacketInt;

BEGIN END MbyMapperStageModel.
