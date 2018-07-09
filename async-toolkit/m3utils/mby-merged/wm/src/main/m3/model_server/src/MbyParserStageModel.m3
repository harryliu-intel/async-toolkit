MODULE MbyParserStageModel;

IMPORT mby_top_map_addr AS TopAddr;

IMPORT mby_ppe_parser_map AS Map;
IMPORT mby_ppe_parser_map_addr AS MapAddr;

IMPORT ServerPacket AS Pkt;
IMPORT Metadata;
IMPORT MbyParserMeta;
IMPORT Coroutine;
IMPORT BaseModelStage;
IMPORT ModelStageResult;


PROCEDURE HandlePacket(from : Coroutine.T;
                       ipkt : Pkt.T;
                       h : TopAddr.H;
                       indices : Indices;
                       imd : Metadata.T;
                       out : ModelStageResult.T) =
  BEGIN
    (* purpose of this routine is to map the records *)
    HandlePacketInt(from,
                    ipkt,
                    h.read  .Mpt[indices.MptIdx].RxPpe.Parser,
                    h.update.Mpt[indices.MptIdx].RxPpe.Parser,
                    imd,
                    out)
  END HandlePacket;
  
PROCEDURE HandlePacketInt(from        : Coroutine.T;
                       ipkt        : Pkt.T;
                       READONLY r  : Map.T;
                       READONLY u  : MapAddr.U;
                       im          : Metadata.T;
                       out         : ModelStageResult.T) =
  BEGIN
    (* duplicate, just to test *)
    FOR i := 1 TO 2 DO
      out.opkt := ipkt;
      out.om := NEW(MbyParserMeta.T);
      EVAL Coroutine.Call(from);
    END;
    out.opkt := NIL (* signal EOT *)
  END HandlePacketInt;

BEGIN END MbyParserStageModel.
