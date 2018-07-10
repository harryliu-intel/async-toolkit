MODULE MbyParserStageModel;

IMPORT mby_top_map_addr AS TopAddr;

IMPORT mby_ppe_parser_map AS Map;
IMPORT mby_ppe_parser_map_addr AS MapAddr;

IMPORT ServerPacket AS Pkt;
IMPORT Metadata;
IMPORT MbyParserMeta;
IMPORT ModelStageResult;


PROCEDURE HandlePacket(ipkt : Pkt.T;
                       h : TopAddr.H;
                       indices : Indices;
                       imd : Metadata.T;
                       out : ModelStageResult.T) =
  BEGIN
    (* purpose of this routine is to map the records *)
    HandlePacketInt(ipkt,
                    h.read  .Mpt[indices.MptIdx].RxPpe.Parser,
                    h.update.Mpt[indices.MptIdx].RxPpe.Parser,
                    imd,
                    out)
  END HandlePacket;
  
PROCEDURE HandlePacketInt(ipkt        : Pkt.T;
                          READONLY r  : Map.T;
                          READONLY u  : MapAddr.U;
                          im          : Metadata.T;
                          out         : ModelStageResult.T) =
  BEGIN
    (* duplicate, just to test *)
    FOR i := 1 TO 2 DO
      out.push(opkt := ipkt, om := NEW(MbyParserMeta.T))
    END
  END HandlePacketInt;

BEGIN END MbyParserStageModel.
