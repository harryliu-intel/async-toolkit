MODULE MbyMapperStageModel EXPORTS MbyMapperStageModel, MbyMapperSizes;

IMPORT mby_top_map_addr AS TopAddr;

IMPORT mby_ppe_mapper_map AS Map;
IMPORT mby_ppe_mapper_map_addr AS MapAddr;

IMPORT ServerPacket AS Pkt;
IMPORT Metadata;
IMPORT MbyParserToMapperMeta;
IMPORT MbyMapperToClassifierMeta;
IMPORT ModelStageResult;
IMPORT MbyMeta;

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
  VAR
    mbm : MbyMeta.T :=
        im.ofType(TYPECODE(MbyMeta.T));
    p2m : MbyParserToMapperMeta.T :=
        im.ofType(TYPECODE(MbyParserToMapperMeta.T));

    isIPv4, isIPv6 := ARRAY [0..NIsIpBits-1] OF BOOLEAN { FALSE, .. };

  BEGIN
    (* we dont actually use the ipkt here *)
    WITH
      portCfg = r.MapPortCfg[mbm.rxPort]
     DO
      
    END;
    out.push(opkt := ipkt, om := NEW(MbyMapperToClassifierMeta.T))
  END HandlePacketInt;

BEGIN END MbyMapperStageModel.
